# [MessagePack](https://msgpack.org/) Emacs Lisp library

msgpack.el is an Emacs Lisp library for decoding and encoding MessagePack.

``` emacs-lisp
(msgpack-read-from-string "\x82\xA5Hello\xA5World\x1\x2")
;; => (("Hello" . "World") (1 . 2))

(equal "\x82\xA5Hello\xA5World\x1\x2"
       (msgpack-encode '(("Hello" . "World") (1 . 2))))
;; => t
```

## API

### `(msgpack-read-from-string STRING)`

Read the MessagePack object in unibyte STRING and return it.

### `(msgpack-read)`

Parse and return the MessagePack object following point.
Advances point just past MessagePack object.

### `(msgpack-encode OBJ)`

Return MessagePack representation of Emacs Lisp OBJ.

## Requirements

- Emacs 25.1
