## 1.1.0

* Added `dommy.core/remove-style!`
* Update `dommy.core/set-attr!` to set more appropriate value for boolean attributes

## 1.0.0

* Updated ClojureScript dependency to 0.0-2356
* BREAKING: Removed `dommy.template` namespace
* BREAKING: Merged `dommy.macros` into `dommy.core`; macros now required like `(:require [dommy.core :as dommy :include-macros true])`
* Merged `dommy.attrs` into `dommy.core`.
* Renamed `dommy.core/ancestor-nodes` to `dommy.core/ancestors`
* Added `dommy.core/create-element` and `dommy.core/create-text-node`

## 0.1.3

* Fix add-class! for when element.classList is not supported (Prismatic/dommy#59)
* Improved support for SVG elements (Prismatic/dommy#60)
* Fixed support attributes containing forward slashes (Prismatic/dommy#62)
* Fix set-style! in some versions of Firefox (Prismatic/dommy#64)

## 0.1.2

## 0.1.1

## 0.1.0

* Consolidated all macros into one ns, dommy.macros
* The following functions were made variadic: add-class!, remove-class!, set-style!, set-px!, set-attr!, and remove-attr!
