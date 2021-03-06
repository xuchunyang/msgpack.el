;;; msgpack-tests.el --- Tests                       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Xu Chunyang

;; Author: Xu Chunyang

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

;; Tests for msgpack.el

;;; Code:

(require 'ert)
(require 'msgpack)
(require 'map)

(ert-deftest msgpack-read ()
  ;; nil, false, true
  (should-not (msgpack-read-from-string (unibyte-string #xc0)))
  (should-not (msgpack-read-from-string (unibyte-string #xc2)))
  (should (msgpack-read-from-string (unibyte-string #xc3)))
  ;; [0, 127]
  (should (zerop (msgpack-read-from-string (unibyte-string 0))))
  (should (= 127 (msgpack-read-from-string (unibyte-string 127))))
  ;; [-32, -1]
  (should (= -32 (msgpack-read-from-string (unibyte-string #b11100000))))
  (should (= -1 (msgpack-read-from-string (unibyte-string #xff))))
  ;; [0, 2^8-1]
  (should (= 0 (msgpack-read-from-string (unibyte-string #xcc 0))))
  (should (= 255 (msgpack-read-from-string (unibyte-string #xcc #xff))))
  ;; [0, 2^16-1]
  (should (= 0 (msgpack-read-from-string (unibyte-string #xcd 0 0))))
  (should (= (1- (expt 2 16)) (msgpack-read-from-string (unibyte-string #xcd #xff #xff))))
  ;; [0, 2^32-1]
  (should (= (1- (expt 2 32)) (msgpack-read-from-string (unibyte-string #xce #xff #xff #xff #xff))))
  ;; [0, 2^64-1]
  (when (> (expt 2 64) 0)               ; need Emacs 27.1's bignum
    (should (= (1- (expt 2 64))
               (msgpack-read-from-string (unibyte-string #xcf #xff #xff #xff #xff #xff #xff #xff #xff)))))
  ;; [-128, 127]
  ;; NOTE https://msgpack.org/index.html's Try! has a bug, it encodes -128 as 3 bytes d1 ff 80
  (should (= -128 (msgpack-read-from-string (unibyte-string #xd0 #x80))))
  ;; [-2^15, 2^15-1]
  (should (= (- (expt 2 15)) (msgpack-read-from-string (unibyte-string #xd1 #x80 #x00))))
  ;; [-2^31, 2^31-1]
  (should (= (- (expt 2 31)) (msgpack-read-from-string (unibyte-string #xd2 #x80 #x00 #x00 #x00))))
  ;; [-2^63, 2^63-1]
  (when (> (expt 2 63) 0)               ; need Emacs 27.1's bignum
    (should (= (- (expt 2 63))
               (msgpack-read-from-string (unibyte-string #xd3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))))
  ;; float
  (should (= 0.15625 (msgpack-read-from-string (unibyte-string #xca #x3e #x20 #x00 #x00))))
  ;; double
  (should (= 0.15625 (msgpack-read-from-string (unibyte-string #xcb #x3f #xc4 #x00 #x00 #x00 #x00 #x00 #x00))))
  ;; string within [0, 31] bytes
  (should (equal "" (msgpack-read-from-string (unibyte-string #b10100000))))
  (should (equal "hello" (msgpack-read-from-string (unibyte-string #b10100101 ?h ?e ?l ?l ?o))))
  ;; string within [0, 255] bytes
  (should (equal (make-string 255 ?x) (msgpack-read-from-string (concat (unibyte-string #xd9 #xff) (make-string 255 ?x)))))
  ;; string within [0, 2^16-1] bytes
  (should (equal "" (msgpack-read-from-string (unibyte-string #xda 0 0))))
  ;; string within [0, 2^32-1] bytes
  (should (equal "" (msgpack-read-from-string (unibyte-string #xdb 0 0 0 0))))
  ;; bin within [0, 255] bytes
  (should (equal (unibyte-string 1 2 3) (msgpack-read-from-string (unibyte-string #xc4 3 1 2 3))))
  ;; bin within [0, 2^16-1] bytes
  (should (equal "" (msgpack-read-from-string (unibyte-string #xc5 0 0))))
  ;; bin within [0, 2^32-1] bytes
  (should (equal "" (msgpack-read-from-string (unibyte-string #xc6 0 0 0 0))))
  ;; array within [0, 15] elements
  (should (equal () (msgpack-read-from-string (unibyte-string #b10010000))))
  (should (equal '(1 2 3) (msgpack-read-from-string (unibyte-string #b10010011 1 2 3))))
  ;; array within [0, 2^16-1] elements
  (should (equal () (msgpack-read-from-string (unibyte-string #xdc 0 0))))
  (should (equal '(1 2 3 4) (msgpack-read-from-string (unibyte-string #xdc 0 4 1 2 3 4))))
  ;; array within [0, 2^32-1] elements
  (should (equal () (msgpack-read-from-string (unibyte-string #xdd 0 0 0 0))))
  ;; map within [0, 15] pairs
  (should (equal () (msgpack-read-from-string (unibyte-string #b10000000))))
  (should (equal '((1 . 2) (3 . 4)) (msgpack-read-from-string (unibyte-string #b10000010 1 2 3 4))))
  (should (equal '((compact . t) (schema . 0))
                 (msgpack-read-from-string (unibyte-string #x82 #xa7 ?c ?o ?m ?p ?a ?c ?t #xc3 #xa6 ?s ?c ?h ?e ?m ?a 0))))
  ;; map within [0, 2^16-1] pairs
  (should (equal () (msgpack-read-from-string (unibyte-string #xde 0 0))))
  (should (equal '((1 . 2) (3 . 4)) (msgpack-read-from-string (unibyte-string #xde 0 2 1 2 3 4))))
  ;; map within [0, 2^32-1] pairs
  (should (equal () (msgpack-read-from-string (unibyte-string #xdf 0 0 0 0))))
  (should (equal '((1 . 2) (3 . 4)) (msgpack-read-from-string (unibyte-string #xdf 0 0 0 2 1 2 3 4))))
  ;; ext, data len = 1 bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xd4 1 2))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal (unibyte-string 2)))))
             t)))
  ;; ext, data len = 2  bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xd5 1 1 2))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal (unibyte-string 1 2)))))
             t)))
  ;; ext, data len = 4 bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xd6 1 1 2 3 4))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal (unibyte-string 1 2 3 4)))))
             t)))
  ;; ext, data len = 8 bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xd7 1 1 2 3 4 5 6 7 8))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal (unibyte-string 1 2 3 4 5 6 7 8)))))
             t)))
  ;; ext, data len = 16 bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xd8 1 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal (unibyte-string 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)))))
             t)))
  ;; ext, data len within [0, 255] bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xc7 0 1))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal ""))))
             t)))
  ;; ext, data len within [0, 2^16-1] bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xc8 0 0 1))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal ""))))
             t)))
  ;; ext, data len within [0, 2^32-1] bytes
  (should (pcase (msgpack-read-from-string (unibyte-string #xc9 0 0 0 0 1))
            ((cl-struct msgpack-ext
                        (type (pred (= 1)))
                        (data (pred (equal ""))))
             t))))

(ert-deftest msgpack-unsigned-to-bytes ()
  (should (equal (msgpack-unsigned-to-bytes #xabcdef 3) (unibyte-string #xab #xcd #xef)))
  (should (equal (msgpack-unsigned-to-bytes #x10100 2) (unibyte-string 1 0))))

(ert-deftest msgpack-signed-to-bytes ()
  ;; [-128, 127]
  (should (equal (msgpack-signed-to-bytes -128 1) (unibyte-string #x80)))
  (should (equal (msgpack-signed-to-bytes -13 1) (unibyte-string #b11110011)))
  (should (equal (msgpack-signed-to-bytes 0 1) (unibyte-string 0)))
  (should (equal (msgpack-signed-to-bytes 127 1) (unibyte-string #x7f))))

(ert-deftest msgpack-byte-to-signed ()
  (should (= (msgpack-byte-to-signed #x80) -128))
  (should (= (msgpack-byte-to-signed #xff) -1))
  (should (= (msgpack-byte-to-signed #x7f) 127)))

(ert-deftest msgpack-encode-integer ()
  ;; [0, 127]
  (cl-loop for i from 0 to 127
           do (should (equal (unibyte-string i)
                             (msgpack-encode-integer i))))
  ;; [-32, -1]
  (should (equal (msgpack-encode-integer -1) (unibyte-string #xff)))
  (should (equal (msgpack-encode-integer -16) (unibyte-string #b11110000)))
  (should (equal (msgpack-encode-integer -32) (unibyte-string #b11100000)))
  ;; [0, 255]
  (should (equal (msgpack-encode-integer 128) (unibyte-string #xcc 128)))
  (should (equal (msgpack-encode-integer 255) (unibyte-string #xcc 255)))
  ;; [0, #xffff]
  (should (equal (msgpack-encode-integer #xffff) (unibyte-string #xcd #xff #xff)))
  ;; [0, #xffffffff]
  (should (equal (msgpack-encode-integer #xffffffff) (unibyte-string #xce #xff #xff #xff #xff)))
  ;; [-128, 127]
  (should (equal (msgpack-encode-integer -128) (unibyte-string #xd0 #x80)))
  (should (equal (msgpack-encode-integer -33) (unibyte-string #xd0 #xdf)))
  ;; [-2^15, 2^15-1]
  (should (equal (msgpack-encode-integer -32768) (unibyte-string #xd1 #x80 0)))
  ;; [-2^31, 2^31-1]
  (should (equal (msgpack-encode-integer (- (expt 2 31))) (unibyte-string #xd2 #x80 #x00 #x00 #x00)))
  ;; [-2^63, 2^63-1]
  (should (equal (msgpack-encode-integer (1- (- (expt 2 31))))
                 (unibyte-string #xd3 #xff #xff #xff #xff #x7f #xff #xff #xff)))
  (unless (zerop (expt 2 63))           ; need Emacs 27.1's bignum
    (should (equal (msgpack-encode-integer (- (expt 2 63)))
                   (unibyte-string #xd3 #x80 #x00 #x00 #x00 #x00 #x00 #x00 #x00)))))

(ert-deftest msgpack-encode-string ()
  ;; string within [0, 31] bytes
  (should (equal (msgpack-encode-string "") (unibyte-string #b10100000)))
  (should (equal (msgpack-encode-string "hello") (unibyte-string #b10100101 ?h ?e ?l ?l ?o)))
  ;; string within [0, 255] bytes
  (should (equal (msgpack-encode-string (make-string 255 ?x))
                 (concat (unibyte-string #xd9 #xff) (make-string 255 ?x))))
  ;; string within [0, 2^16-1] bytes
  (should (equal (msgpack-encode-string (make-string 256 ?x))
                 (concat (unibyte-string #xda 1 0) (make-string 256 ?x))))
  ;; string within [0, 2^32-1] bytes (upto 4 GB)
  (should (equal (msgpack-encode-string (make-string (expt 2 16) ?x))
                 (concat (unibyte-string #xdb 0 1 0 0) (make-string (expt 2 16) ?x)))))

(ert-deftest msgpack-encode-unibyte-string ()
  ;; bin within [0, 255] bytes
  (should (equal (msgpack-encode-unibyte-string "") (unibyte-string #xc4 0)))
  (should (equal (msgpack-encode-unibyte-string "hi") (unibyte-string #xc4 2 ?h ?i)))
  ;; bin within [0, 2^16-1] bytes
  (should (equal (msgpack-encode-unibyte-string (make-string 256 ?x))
                 (concat (unibyte-string #xc5 1 0) (make-string 256 ?x))))
  ;; bin within [0, 2^32-1] bytes
  (should (equal (msgpack-encode-unibyte-string (make-string (expt 2 16) ?x))
                 (concat (unibyte-string #xc6 0 1 0 0) (make-string (expt 2 16) ?x)))))

(ert-deftest msgpack-float-to-bytes ()
  (should (equal (msgpack-float-to-bytes 0.15625) (unibyte-string #x3e #x20 #x00 #x00)))
  (should (equal (msgpack-float-to-bytes 1.1) (unibyte-string #x3f #x8c #xcc #xcd)))
  (should (equal (msgpack-float-to-bytes -1.1) (unibyte-string #xbf #x8c #xcc #xcd)))
  (should (equal (msgpack-float-to-bytes 1.7) (unibyte-string #x3f #xd9 #x99 #x9a)))
  (should (equal (msgpack-float-to-bytes 123.456) (unibyte-string #x42 #xf6 #xe9 #x79)))
  (should (equal (msgpack-float-to-bytes .9999999) (unibyte-string #x3f #x7f #xff #xfe)))
  (should (equal (msgpack-bytes-to-hex-string (msgpack-float-to-bytes 3.14)) "4048f5c3")))

(ert-deftest msgpack-encode-array ()
  (should (equal (msgpack-encode-array ()) (unibyte-string #x90)))
  (should (equal (msgpack-encode-array '(1 2 3)) (unibyte-string #b10010011 1 2 3))))

(ert-deftest msgpack-encode-alist ()
  (should (equal (msgpack-encode-alist ()) (unibyte-string #x80)))
  (should (equal (msgpack-encode-alist '((1 . 2) (3 . 4))) (unibyte-string #b10000010 1 2 3 4))))

(ert-deftest msgpack-encode-plist ()
  (should (equal (msgpack-encode-plist '(1 2 3 4)) (unibyte-string #b10000010 1 2 3 4)))
  (should (equal (msgpack-encode-plist '(:a 1 :b 2))
                 (unibyte-string #b10000010 #b10100001 ?a 1 #b10100001 ?b 2))))

(ert-deftest msgpack-encode-list ()
  (should (equal (msgpack-encode-list '((1 . 2) (3 . 4))) (unibyte-string #b10000010 1 2 3 4)))
  (should (equal (msgpack-encode-list '(1 2 3 4)) (unibyte-string #b10010100 1 2 3 4)))
  (should (equal (msgpack-encode-list '(:a 1 :b 2))
                 (unibyte-string #b10000010 #b10100001 ?a 1 #b10100001 ?b 2))))

(ert-deftest msgpack-encode-ext ()
  (should (equal (msgpack-encode-ext (msgpack-ext-make 42 "ABCD"))
                 (msgpack-concat #xd6 42 "ABCD")))
  (should (equal (msgpack-encode-ext (msgpack-ext-make 42 "ABC"))
                 (msgpack-concat #xc7 3 42 "ABC")))
  (should (equal (msgpack-encode-ext (msgpack-ext-make -42 "ABC"))
                 (msgpack-concat #xc7 3 -42 "ABC"))))

(ert-deftest msgpack-encode ()
  (should (equal (msgpack-bytes-to-hex-string (msgpack-encode nil)) "c0"))
  (should (equal (msgpack-bytes-to-hex-string (msgpack-encode :msgpack-false)) "c2"))
  (should (equal (msgpack-bytes-to-hex-string (msgpack-encode t)) "c3"))
  (should (equal (msgpack-encode t) (unibyte-string #xc3)))
  (should (equal (msgpack-encode '(("compact" . t) ("schema" . 0)))
                 (msgpack-concat #x82 #xa7 "compact" #xc3 #xa6 "schema" 0)))
  (should (equal (msgpack-encode #s(hash-table test equal size 2 data (compact t schema 0)))
                 (msgpack-concat #x82 #xa7 "compact" #xc3 #xa6 "schema" 0)))
  (should (equal (msgpack-encode []) (unibyte-string #x90)))
  (should (equal (msgpack-encode (msgpack-bin-make "bin"))
                 (msgpack-concat #xc4 3 "bin")))
  (should (equal (msgpack-encode (msgpack-ext-make 0 "x"))
                 (msgpack-concat #xd4 0 "x")))
  (should (equal (msgpack-encode :key)
                 (msgpack-concat #b10100011 "key")))
  (should (equal (msgpack-encode 'key)
                 (msgpack-concat #b10100011 "key"))))

(ert-deftest msgpack-try-read ()
  (should (progn (msgpack-try-read-from-string "\xa5hello") t))
  (should-error (msgpack-try-read-from-string "\xa5hell") :type 'end-of-buffer)
  (should-error (msgpack-try-read-from-string
                 (msgpack-concat
                  #x82                  ; map, 2 pairs
                  #xa7 "compact"        ; str
                  #xc3                  ; bool, t
                  #xa6 "schema"))       ; str
                :type 'end-of-buffer))

(ert-deftest msgpack-array-type ()
  (should (listp (msgpack-read-from-string (msgpack-encode [1 2 3]))))
  (should (let ((msgpack-array-type 'vector))
            (vectorp (msgpack-read-from-string (msgpack-encode [1 2 3]))))))

(ert-deftest msgpack-read-map ()
  (should (equal (msgpack-read-from-string (msgpack-concat #x82 #xa7 "compact" #xc3 #xa6 "schema" 0))
                 '((compact . t) (schema . 0))))
  (should (equal (let ((msgpack-map-type 'plist))
                   (msgpack-read-from-string (msgpack-concat #x82 #xa7 "compact" #xc3 #xa6 "schema" 0)))
                 '(:compact t :schema 0)))
  (should (hash-table-p
           (let ((msgpack-map-type 'hash-table))
             (msgpack-read-from-string (msgpack-concat #x82 #xa7 "compact" #xc3 #xa6 "schema" 0)))))
  (should (equal (msgpack-read-from-string (msgpack-concat #x82 1 2 3 4))
                 '((1 . 2) (3 . 4)))))

(ert-deftest msgpack-read-file ()
  (let ((x '("hello" 42 "Unicode test 中文")))
    (let ((tmpfile (make-temp-file "msgpack-tests-")))
      (write-region (msgpack-encode x) nil tmpfile)
      (unwind-protect
          (should (equal x (msgpack-read-file tmpfile)))
        (delete-file tmpfile)))))

(ert-deftest msgpack-read-timestamp ()
  (should (= 1584211079
             (time-to-seconds
              (msgpack-read-from-string
               (unibyte-string #xd6 #xff #x5E #x6D #x24 #x87)))))

  ;; $ gdate +%s:%N
  ;; 1584216254:357957000
  (let ((seconds 1584216254)
        (nanoseconds 357957000))
    (pcase-exhaustive (msgpack-read-from-string
                       (msgpack-concat
                        #xd7 -1
                        (msgpack-bits-to-bytes
                         (nconc
                          (msgpack-list-pad-left (msgpack-unsigned-to-bits nanoseconds) 30 0)
                          (msgpack-list-pad-left (msgpack-unsigned-to-bits seconds) 34 0)))))
      (`(,high ,low ,micro ,pico)
       (should (= seconds (+ (* high (expt 2 16)) low)))
       (should (= nanoseconds (+ (* 1000 micro) pico))))))

  (let ((seconds 1584216254)
        (nanoseconds 357957000))
    (pcase-exhaustive (msgpack-read-from-string
                       (msgpack-concat
                        #xc7 12 -1
                        (msgpack-unsigned-to-bytes nanoseconds 4)
                        (msgpack-unsigned-to-bytes seconds 8)))
      (`(,high ,low ,micro ,pico)
       (should (= seconds (+ (* high (expt 2 16)) low)))
       (should (= nanoseconds (+ (* 1000 micro) pico)))))))

(ert-deftest msgpack-bits-plus-one ()
  (should (equal (msgpack-bits-plus-one '(1 0 0 0 1 0)) '(1 0 0 0 1 1)))
  (should (equal (msgpack-bits-plus-one '(1 0 0 0 1 1)) '(1 0 0 1 0 0)))
  (should (equal (msgpack-bits-plus-one '(1 0 0 1 1 1)) '(1 0 1 0 0 0)))
  (should (equal (msgpack-bits-plus-one '(1 1 1 1 1 1)) '(0 0 0 0 0 0))))

(ert-deftest msgpack-signed-to-bytes-fallback ()
  (should (equal (msgpack-signed-to-bytes-fallback -1 1) (unibyte-string #xff)))
  (should (equal (msgpack-signed-to-bytes-fallback -1 4) (unibyte-string #xff #xff #xff #xff)))
  (should (equal (msgpack-signed-to-bytes-fallback -1 8) (unibyte-string #xff #xff #xff #xff #xff #xff #xff #xff)))
  (should (equal (msgpack-signed-to-bytes-fallback -30 4) (unibyte-string #xff #xff #xff #xe2)))
  (should (equal (msgpack-signed-to-bytes-fallback -30 8) (unibyte-string #xff #xff #xff #xff #xff #xff #xff #xe2))))

(provide 'msgpack-tests)
;;; msgpack-tests.el ends here
