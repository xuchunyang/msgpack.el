# MessagePack Emacs Lisp library
[![Melpa](https://melpa.org/packages/msgpack-badge.svg)](https://melpa.org/#/msgpack)

msgpack.el is an Emacs Lisp library for reading and writing [MessagePack](https://msgpack.org/).

``` emacs-lisp
(msgpack-read-from-string "\xA5Hello")
;; => "Hello"

(msgpack-encode "Hello")
;; => "\245Hello"
```

## API

### `(msgpack-read-from-string STRING)`

Read the MessagePack object in unibyte STRING and return it.

### `(msgpack-read)`

Parse and return the MessagePack object following point.
Advances point just past MessagePack object.

### `(msgpack-encode OBJ)`

Return MessagePack representation of OBJ.

### `msgpack-false` (defaults to `:msgpack-false`)

### `msgpack-null` (defaults to `nil`)

### `msgpack-array-type` (defaults to `list`)

### `msgpack-map-type` (defaults to `alist`)

### `msgpack-key-type` (defaults to `nil`)

## Requirements

- Emacs 25.1
