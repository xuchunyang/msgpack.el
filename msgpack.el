;;; msgpack.el --- MessagePack Library               -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang
;; Homepage: https://github.com/xuchunyang/msgpack.el
;; Package-Requires: ((emacs "25.1"))
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

(require 'json)                         ; `json-alist-p'
(require 'cl-lib)

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
      ;; XXX timestamps
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

(defun msgpack-unsigned-to-bytes (integer size)
  "Convert unsigned INTEGER to SIZE bytes."
  (apply
   #'unibyte-string
   (nreverse
    (cl-loop repeat size
             for i from 0 by 8
             collect (logand #xff (lsh integer (- i)))))))

(defun msgpack-signed-to-bytes (integer size)
  "Convert signed INTEGER to SIZE bytes."
  (if (>= integer 0)
      (msgpack-unsigned-to-bytes integer size)
    (msgpack-unsigned-to-bytes (lognot (1- (- integer))) size)))

(defun msgpack-encode-integer (n)
  "Return a MessagePack representation of integer N."
  (pcase n
    ((guard (<= 0 n 127))
     (unibyte-string n))
    ((guard (<= -32 n -1))
     (unibyte-string (logior #b11100000 (+ 32 n))))
    ((guard (<= 0 n 255))
     (unibyte-string #xcc n))
    ((guard (<= 0 n #xffff))
     (concat (unibyte-string #xcd) (msgpack-unsigned-to-bytes n 2)))
    ;; NOTE #xffffffff or 2^32 overflow for 32-bit platform
    ((guard (<= 0 n #xffffffff))
     (concat (unibyte-string #xce) (msgpack-unsigned-to-bytes n 4)))
    ((or (guard (and (> (expt 2 64) 0)  ; need Emacs 27.1's bignum
                     (<= 0 n (1- (expt 2 64)))))
         (guard (<= 0 n (max (1- (expt 2 64)) most-positive-fixnum))))
     (concat (unibyte-string #xcf) (msgpack-unsigned-to-bytes n 8)))
    ((guard (<= -128 n 127))
     (concat (unibyte-string #xd0) (msgpack-signed-to-bytes n 1)))
    ((guard (<= (- (expt 2 15)) n (1- (expt 2 15))))
     (concat (unibyte-string #xd1) (msgpack-signed-to-bytes n 2)))
    ((guard (<= (- (expt 2 31)) n (1- (expt 2 31))))
     (concat (unibyte-string #xd2) (msgpack-signed-to-bytes n 4)))
    ((guard (or (and (> (expt 2 63) 0)  ; need Emacs 27.1's bignum
                     (<= (- (expt 2 63)) n (1- (expt 2 63))))
                (<= (min most-negative-fixnum (- (expt 2 63)))
                    n
                    (max most-positive-fixnum (1- (expt 2 63))))))
     (concat (unibyte-string #xd3) (msgpack-signed-to-bytes n 8)))))

(defun msgpack-encode-string (string)
  "Return a MessagePack representation of UTF-8 STRING."
  (let ((n (string-bytes string))
        (s (encode-coding-string string 'utf-8)))
    (cond
     ((<= n 31)
      (concat (unibyte-string (logior #b10100000 n)) s))
     ((<= n #xff)
      (concat (unibyte-string #xd9) (msgpack-unsigned-to-bytes n 1) s))
     ((<= n (1- (expt 2 16)))
      (concat (unibyte-string #xda) (msgpack-unsigned-to-bytes n 2) s))
     ((<= n (1- (expt 2 32)))
      (concat (unibyte-string #xdb) (msgpack-unsigned-to-bytes n 4) s)))))

(defun msgpack-encode-unibyte-string (string)
  "Return a MessagePack representation of unibyte STRING."
  (cl-assert (not (multibyte-string-p string)))
  (let ((n (length string))
        (s string))
    (cond
     ((<= n 255)
      (concat (unibyte-string #xc4) (msgpack-unsigned-to-bytes n 1) s))
     ((<= n #xffff)
      (concat (unibyte-string #xc5) (msgpack-unsigned-to-bytes n 2) s))
     ((<= n (1- (expt 2 32)))
      (concat (unibyte-string #xc6) (msgpack-unsigned-to-bytes n 4) s)))))

(defun msgpack-unsigned-to-bits (n)
  "Convert unsigned integer N to bits."
  (if (zerop n)
      (list 0)
    (let (next bits)
      (while (> n 0)
        (setq next (/ n 2))
        (push (- n (* 2 next)) bits)
        (setq n next))
      bits)))

(defun msgpack-split-float (f)
  "Split float F into integral and fractional parts."
  (let ((integral (truncate f)))
    (list integral
          ;; (- 1.1 1)
          ;; => 0.10000000000000009
          ;; https://0.30000000000000004.com/
          (- f integral))))

(defun msgpack-split-float-the-hard-way (f)
  "Split float F into integral and fractional parts."
  (let* ((s (prin1-to-string f))
         (pos (cl-position ?. s)))
    (list (car (read-from-string s 0 pos))
          (car (read-from-string s pos)))))

(defun msgpack-float-to-bits (f limit)
  "Convert float F to at most LIMIT bits."
  (let (bits double (n 0))
    (while (and (not (zerop f)) (< n limit))
      (setq double (* f 2))
      (cond
       ((>= double 1)
        (push 1 bits)
        (setq f (1- double)))
       (t
        (push 0 bits)
        (setq f double)))
      (cl-incf n))
    (nreverse bits)))

(defun msgpack-float-to-bits-normalize (ibits fbits)
  (let* ((index (length ibits))
         (bits (append ibits fbits))
         (e (1- (- (length ibits) (cl-position 1 bits)))))
    (setq index (- index e))
    (list (cl-subseq bits index) e)))

(defun msgpack-list-pad-right (list len padding)
  "If LIST is shorter than LEN, pad it with PADDING on the right."
  (pcase (- len (length list))
    ((and (pred (< 0)) diff) (append list (make-list diff padding)))
    (_ list)))

(defun msgpack-list-pad-left (list len padding)
  "If LIST is shorter than LEN, pad it with PADDING on the left."
  (pcase (- len (length list))
    ((and (pred (< 0)) diff) (append (make-list diff padding) list))
    (_ list)))

(defun msgpack-8bits-to-byte (8bits)
  "Convert 8BITS to a byte."
  (cl-loop for i in 8bits
           for j from 7 downto 0
           sum (* i (expt 2 j))))

(defun msgpack-bits-to-bytes (bits)
  "Convert BITS to bytes."
  ;; (cl-assert (zerop (% (length bits) 8)))
  (cl-loop for i from 0 to (1- 32) by 8
           concat (unibyte-string (msgpack-8bits-to-byte (cl-subseq bits i (+ i 8))))))

;; Emacs float uses IEEE 64-bit but we can't asscess it from Emacs Lisp
;; XXX File a feature request
(defun msgpack-float-to-bytes (f)
  "Convert float F to IEEE 32-bit."
  (pcase-let* ((sign (if (< f 0) 1 0))
               (f (abs f))
               (`(,int ,frac) (msgpack-split-float-the-hard-way f))
               (ibits (msgpack-unsigned-to-bits int))
               (fbits (msgpack-float-to-bits frac 32)) ; XXX why 32?
               (`(,bits ,e) (msgpack-float-to-bits-normalize ibits fbits))
               (mantissa (msgpack-list-pad-right bits 23 0))
               (exponent (msgpack-list-pad-left (msgpack-unsigned-to-bits (+ e 127)) 8 0)))
    (msgpack-bits-to-bytes (append (list sign) exponent mantissa))))

(defun msgpack-encode-float (f)
  "Encode float F as MessagePack float."
  (concat (unibyte-string #xca) (msgpack-float-to-bytes f)))

(defun msgpack-encode-list (list)
  "Encode LIST as MessagePack array."
  (let ((n (length list)))
    (cond
     ((<= n 15)
      (concat (unibyte-string (logior #b10010000 n))
              (mapconcat #'msgpack-encode list "")))
     ((<= n #xffff)
      (concat (unibyte-string #xdc)
              (msgpack-unsigned-to-bytes n 2)
              (mapconcat #'msgpack-encode list "")))
     ((<= n (1- (expt 2 32)))
      (concat (unibyte-string #xdd)
              (msgpack-unsigned-to-bytes n 4)
              (mapconcat #'msgpack-encode list ""))))))

(defun msgpack-encode-alist (alist)
  "Encode ALIST as MessagePack map."
  (let ((n (length alist)))
    (concat
     (cond
      ((<= n 15)
       (unibyte-string (logior #b10000000 n)))
      ((<= n #xffff)
       (unibyte-string #xde (msgpack-unsigned-to-bytes n 2)))
      ((<= n (1- (expt 2 32)))
       (unibyte-string #xdf (msgpack-unsigned-to-bytes n 4))))
     (cl-loop for (k . v) in alist
              concat (concat
                      (msgpack-encode (if (symbolp k) (symbol-name k) k))
                      (msgpack-encode v))))))

(defun msgpack-encode (obj)
  "Return MessagePack representation of Emacs Lisp OBJ."
  (pcase obj
    ('t (unibyte-string #xc3))
    ('nil (unibyte-string #xc2))
    ((pred integerp) (msgpack-encode-integer obj))
    ((and (pred floatp) (pred zerop)) (msgpack-encode-integer obj))
    ((pred floatp) (msgpack-encode-float obj))
    ((pred vectorp) (msgpack-encode-unibyte-string (concat obj)))
    ((pred stringp) (msgpack-encode-string obj))
    ((and (pred listp) (pred json-alist-p)) (msgpack-encode-alist obj))
    ((pred listp) (msgpack-encode-list obj))))

(provide 'msgpack)
;;; msgpack.el ends here
