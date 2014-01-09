<img src="resources/logo.png" width="270" />

A ClojureScript DOM manipulation, templating and event library.

## Usage

Add the following dependency to your `project.clj`:

```clojure
[prismatic/dommy "0.1.2"]
```

### Selection

DOM nodes are selected using macros, which expand to the correct native dom calls. Because selectors don't wrap returned nodes, there is a distinction between single and multiple selections. A selector can be a keyword, string or vector.

```clojure
(ns …
  (:require 
    [dommy.utils :as utils]
    [dommy.core :as dommy])
  (:use-macros
    [dommy.macros :only [node sel sel1]]))

(sel1 :body) ; => document.body
(sel1 :#header) ; => document.getElementById("header")
(sel1 ".todo") ; => document.getElementsByClassName("todo")[0]

(sel [:#todos :li]) ; => document.querySelectorAll("#todos li")
```

### DOM Manipulation

Inspired by [jQuery](http://jquery.com), but adapted to be functional in order to better fit with ClojureScript core.

```clojure
(dommy/append! (sel1 :#todos) [:.todo "Eat some cake"])

(doseq [todo (sel :.todo)]
  (dommy/add-class! todo :complete))

(map dommy/text (sel :.todo))
```

Functions that modify take the target dom node as their first argument, and return the same modified node, allowing the use of threading macros to accomplish jQuery-like chaining.

```clojure
(-> (sel1 :#my-button)
	(dommy/remove-attr! :disabled)
	(dommy/add-class! :active)
	(dommy/set-text! "Click me!"))

(->> (sel :.image)
	 (filter #(> (dommy/px % :width) 500))
	 (map #(dommy/add-class! % :big-image)))
```

Dom manipulation is defined in [dommy.core](https://github.com/Prismatic/dommy/blob/master/src/dommy/core.cljs) and [dommy.attrs](https://github.com/Prismatic/dommy/blob/master/src/dommy/attrs.cljs).

### Templating

Templating syntax is based on [Hiccup](https://github.com/weavejester/hiccup/), a great HTML library for Clojure. Instead of returning a string of html, dommy's `node` macro returns a DOM node.

```clojure
(ns …
  (:require [dommy.core])
  (:use-macros
    [dommy.macros :only [node]]))

(node
  [:div#id.class1
    (for [r (range 2)]
      [:span.text (str "word" r)])]) ;; => [object HTMLElement]

;; Styles can be inlined as a map
(node
  [:span
    {:style
      {:color "#aaa"
       :text-decoration "line-through"}}])
```

The `deftemplate` macro is useful syntactic sugar for defining a function that returns a DOM node.

```clojure
(ns …
  (:require [dommy.core])
  (:use-macros
    [dommy.macros :only [node deftemplate]]))

(defn simple-template [cat]
  (node [:img {:src cat}]))

(deftemplate simple-template [cat]
  [:img {:src cat}])
```

Thanks to [@ibdknox](https://github.com/ibdknox/), you can define view logic for custom types by implementing the `PElement` protocol:

```clojure
(defrecord MyModel [data]
   dommy.template/PElement
   (-elem [this] (node [:p (str "My data " data)])))

(dommy/append! (sel1 :body) (MyModel. "is big"))
```

### Type-Hinting Template Macros

One caveat of using the compile-macro is that if you have a compound element (a vector element) and want to have a non-literal map as the attributes (the second element of the vector), then you need to use <code>^:attrs</code> meta-data so the compiler knows to process this symbol as a map of attributes in the runtime system. Here's an example:

```clojure
(node [:a ^:attrs (merge m1 m2)])
```

## Testing

Dommy comes with reasonably extensive tests. To run them 
first build the `test` cljsbuild target, as follows:

    $ lein clean
    $ lein cljsbuild auto test

Next, open up the HTML file under `resources/dommy-tests.html` which will give you a visual representation of all tests. For
all pull requests, please ensure your tests pass (or add test cases) before submitting. 


## License

Copyright (C) 2013 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
