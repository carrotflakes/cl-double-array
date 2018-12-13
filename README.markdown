# cl-double-array

## Usage

``` lisp
(defvar words '("a" "to" "tea" "ted" "ten" "i" "in" "inn" "int" "inter" "internet"))

(defvar double-array (build-double-array words))

(common-prefix-search double-array "internet!")
; => ("i" "in" "int" "inter" "internet")

```

## Installation

```
ros install carrotflakes/cl-double-array
```

## Author

* carrotflakes (carrotflakes@gmail.com)

## Copyright

Copyright (c) 2018 carrotflakes (carrotflakes@gmail.com)

## License

Licensed under the LLGPL License.
