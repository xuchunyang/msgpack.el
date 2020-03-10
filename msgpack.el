;;; msgpack.el --- MessagePack Library               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/msgpack.el
;; Package-Requires: ((emacs "24.5"))
;; Keywords: lisp
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs Lisp library for MessagePack <https://msgpack.org/index.html>

;;; Code:

(defun msgpack-read-byte ()
  "Read one byte."
  (prog1 (following-char)
    (forward-char 1)))

(defun msgpack-read-bytes (amt)
  "Read AMT bytes."
  (prog1 (buffer-substring-no-properties (point) (+ (point) amt))
    (forward-char amt)))

(defun msgpack-byte-to-bits (byte)
  "Convert 1 BYTE to a list of 8 bits."
  (cl-loop for i from 7 downto 0
           collect (if (= (logand byte (expt 2 i)) 0)
                       0
                     1)))

(defun msgpack-bits-to-unsigned (bits)
  "Convert BITS to unsigned."
  (cl-loop for i from (1- (length bits)) downto 0
           for b in bits
           sum (* b (expt 2 i))))

(defun msgpack-bytes-to-unsigned (bytes)
  "Convert BYTES to unsigned int."
  (cl-loop for i from 0
           for n across (nreverse bytes)
           sum (* n (expt (expt 2 8) i))))

(defun msgpack-bytes-to-signed (bytes)
  "Convert BYTES to signed int."
  (let ((nbits (* 8 (length bytes)))
        (num (msgpack-bytes-to-unsigned bytes)))
    (pcase (lsh num (- (1- nbits)))     ; sign
      (0 num)
      (1 (- num (expt 2 nbits))))))

(defun msgpack-bytes-to-bits (bytes)
  "Convert BYTES to bits."
  (cl-mapcan #'msgpack-byte-to-bits bytes))

(defun msgpack-bytes-to-float (bytes)
  "Convert BYTES to IEEE 754 float."
  (let* ((bits (msgpack-bytes-to-bits bytes))
         (sign (car bits))
         (e (msgpack-bits-to-unsigned (cl-subseq bits 1 9)))
         (fraction (1+ (cl-loop for b in (cl-subseq bits 9)
                                for i from 1 to 23
                                sum (* b (expt 2 (- i)))))))
    (* (expt -1 sign) (expt 2 (- e 127)) fraction)))

(defun msgpack-bytes-to-double (bytes)
  "Convert BYTES to IEEE 754 double."
  (let* ((bits (msgpack-bytes-to-bits bytes))
         (sign (car bits))
         (e (msgpack-bits-to-unsigned (cl-subseq bits 1 12)))
         (fraction (1+ (cl-loop for b in (cl-subseq bits 12)
                                for i from 1 to 52
                                sum (* b (expt 2 (- i)))))))
    (* (expt -1 sign) (expt 2 (- e 1023)) fraction)))

(cl-defstruct (msgpack-ext (:constructor nil)
                           (:constructor msgpack-ext-make (type data))
                           (:copier nil))
  type data)

(defun msgpack-read ()
  "Parse and return the MessagePack object following point.
Advances point just past MessagePack object."
  (let ((b (msgpack-read-byte)))
    (pcase b
      ;; nil
      (#xc0 nil)
      (#xc1 (error "Never used: #xc1"))
      ;; bool
      (#xc2 nil)
      (#xc3 t)
      ;; int
      (#xcc (msgpack-read-byte))
      (#xcd (msgpack-bytes-to-unsigned (msgpack-read-bytes 2)))
      (#xce (msgpack-bytes-to-unsigned (msgpack-read-bytes 4)))
      (#xcf (msgpack-bytes-to-unsigned (msgpack-read-bytes 8)))
      (#xd0 (msgpack-bytes-to-signed (msgpack-read-bytes 1)))
      (#xd1 (msgpack-bytes-to-signed (msgpack-read-bytes 2)))
      (#xd2 (msgpack-bytes-to-signed (msgpack-read-bytes 4)))
      (#xd3 (msgpack-bytes-to-signed (msgpack-read-bytes 8)))
      ;; float
      (#xca (msgpack-bytes-to-float (msgpack-read-bytes 4)))
      (#xcb (msgpack-bytes-to-double (msgpack-read-bytes 8)))
      ;; string
      (#xd9 (decode-coding-string (msgpack-read-bytes (msgpack-read-byte)) 'utf-8))
      (#xda (decode-coding-string (msgpack-read-bytes (msgpack-bytes-to-unsigned (msgpack-read-bytes 2))) 'utf-8))
      (#xdb (decode-coding-string (msgpack-read-bytes (msgpack-bytes-to-unsigned (msgpack-read-bytes 4))) 'utf-8))
      ;; bin
      (#xc4 (msgpack-read-bytes (msgpack-read-byte)))
      (#xc5 (msgpack-read-bytes (msgpack-bytes-to-unsigned (msgpack-read-bytes 2))))
      (#xc6 (msgpack-read-bytes (msgpack-bytes-to-unsigned (msgpack-read-bytes 4))))
      ;; array
      (#xdc (cl-loop repeat (msgpack-bytes-to-unsigned (msgpack-read-bytes 2))
                     collect (msgpack-read)))
      (#xdd (cl-loop repeat (msgpack-bytes-to-unsigned (msgpack-read-bytes 4))
                     collect (msgpack-read)))
      ;; map
      (#xde (cl-loop repeat (msgpack-bytes-to-unsigned (msgpack-read-bytes 2))
                     collect (cons (msgpack-read) (msgpack-read))))
      (#xdf (cl-loop repeat (msgpack-bytes-to-unsigned (msgpack-read-bytes 4))
                     collect (cons (msgpack-read) (msgpack-read))))
      ;; ext
      (#xd4 (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes 1)))
      (#xd5 (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes 2)))
      ;; xxx timestamps
      (#xd6 (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes 4)))
      (#xd7 (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes 8)))
      (#xd8 (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes 16)))
      (#xc7 (let ((len (msgpack-read-byte)))
              (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes len))))
      (#xc8 (let ((len (msgpack-bytes-to-unsigned (msgpack-read-bytes 2))))
              (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes len))))
      (#xc9 (let ((len (msgpack-bytes-to-unsigned (msgpack-read-bytes 4))))
              (msgpack-ext-make (msgpack-read-byte) (msgpack-read-bytes len))))
      (_ (pcase (msgpack-byte-to-bits b)
           (`(0 . ,_) b)
           (`(1 1 1 . ,bits) (- (msgpack-bits-to-unsigned bits) (expt 2 5)))
           (`(1 0 1 . ,bits) (decode-coding-string (msgpack-read-bytes (msgpack-bits-to-unsigned bits)) 'utf-8))
           (`(1 0 0 1 . ,bits) (cl-loop repeat (msgpack-bits-to-unsigned bits)
                                        collect (msgpack-read)))
           (`(1 0 0 0 . ,bits) (cl-loop repeat (msgpack-bits-to-unsigned bits)
                                        collect (cons (msgpack-read) (msgpack-read)))))))))

(defun msgpack-read-from-string (string)
  "Read the MessagePack object in unibyte STRING and return it."
  (cl-assert (not (multibyte-string-p string)))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert string)
    (goto-char (point-min))
    (msgpack-read)))

(provide 'msgpack)
;;; msgpack.el ends here
