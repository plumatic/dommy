## 1.0.0

* Updated ClojureScript dependency to 0.0-2356
* Removed all templating-related functionality (`node`, `deftemplate`, etc)
* Removed `dommy.template`
* Consolidated all core functionality to `dommy.core`, removing `dommy.attrs`
* Renamed `dommy.macros` to `dommy.core`, allowing `(:require [dommy.core :as dommy :include-macros true])`
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
