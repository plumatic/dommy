<img src="resources/logo.png" width="270" />

A ClojureScript DOM manipulation and event library.

## Usage

Add the following dependency to your `project.clj`:

```clojure
[prismatic/dommy "1.1.0"]
```

#### Upgrading to 1.0.0+ from 0.X.Y

Version "1.0.0" includes breaking API changes. Here's a quick overview of what's
changed:

*   `dommy.template` namespace and all related templating features
    (`node`, `deftemplate`, etc) have been removed from the library.
*   Simplified namespace structure.
    Everything in `dommy.macros` and `dommy.attrs` has been moved into
    `dommy.core`

See CHANGELOG.md or <https://github.com/plumatic/dommy/pull/85> for more
details.

If you are looking for hiccup-style templating features, check out https://github.com/jeluard/hipo/

### Selection

DOM nodes are selected using macros, which expand to the correct native dom calls. Because selectors don't wrap returned nodes, there is a distinction between single and multiple selections. A selector can be a keyword, string or vector.

```clojure
(ns foo.bar
  (:require 
    [dommy.core :refer-macros [sel sel1]]))

(sel1 :body) ; => document.body
(sel1 :#header) ; => document.getElementById("header")
(sel1 ".todo") ; => document.getElementsByClassName("todo")[0]

(sel [:#todos :li]) ; => document.querySelectorAll("#todos li")
```

Sometimes its useful to specify the base node from which the selection takes place.

```clojure
(sel1 todos-element :.todo)
```

### DOM Manipulation

Inspired by [jQuery](http://jquery.com), but adapted to be functional in order to better fit with ClojureScript core.

```clojure
(dommy/append! (sel1 :#todos) todo-element)

(doseq [todo (sel :.todo)]
  (dommy/add-class! todo :complete))

(map dommy/text (sel :.todo))
```

Functions that modify take the target node as their first argument, and return the same modified node, allowing the use of threading macros to accomplish jQuery-like chaining.

```clojure
(-> (sel1 :#my-button)
	(dommy/remove-attr! :disabled)
	(dommy/add-class! :active)
	(dommy/set-text! "Click me!"))

(->> (sel :.image)
	 (filter #(> (dommy/px % :width) 500))
	 (map #(dommy/add-class! % :big-image)))
```

### Events

Dommy's event API closely resembles the native JavaScript API.

```clojure
(ns foo.bar
  (:require
    [dommy.core :as dommy :refer-macros [sel1]]))

(defn click-handler [e]
    (.log js/console "You clicked my button! Congratulations"))

(dommy/listen! (sel1 :#my-button) :click click-handler)

(dommy/unlisten! (sel1 :#my-button) :click click-handler)
```

If the first argument to `listen!` is a sequence, the handler will delegate events to the selected element defined by the sequence. A special property `selectedTarget` added to the event specifies the element selected.

```clojure
(defn todo-click-handler [e]
  (.log js/console "The clicked todo is" (.-selectedTarget e)))

(dommy/listen! [todos-element :.todo] :click todo-click-handler))
```

## Testing

For all pull requests, please ensure your tests pass (or add test cases) before submitting. 

    $ lein cljsbuild test

## License

Copyright (C) 2014 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
