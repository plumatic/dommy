# dommy

Dommy is no-nonsense ClojureScript DOM templating and manipulation library. Templating is based on Clojure's [Hiccup](https://github.com/weavejester/hiccup/) html templating library. It is similar to [Crate](https://github.com/ibdknox/crate), but is much faster (3-4x, see the performance comparison test `dommy.template-perf-test`). It also has a compile-time macro component that is significantly (5x) faster than the runtime templating, but requires most of your DOM structure to be expressed as nested vector literals (see 'Compile Macros' below). The compile-time macros are roughly 3x the speed of jQuery's templates and 12x faster than Crate.  

DOM manipulation is inspired by jQuery, but adapted to be more Clojure-y and is also significantly faster (roughly 2x) for common tasks like toggling or adding classes (see this [performance test](https://github.com/Prismatic/dommy/tree/master/test/dommycore_test.clj)).


## Installation

Add the following dependency on your project.clj:

```clojure
;; latest stable release
[prismatic/dommy "0.1.0"]
```

## Templating Usage


```clojure
(ns awesome-webapp
 (:require [dommy.template :as template]))

(template/node
  [:div#id.class1
    (for [r (range 2)]
      [:span.text (str "word" r)])])

=> [object HTMLElement]

(.-outerHTML *1)

=> "<div id=\"id\" class=\"class1\"><span class=\"text\">word0</span><span class=\"text\">word1</span></div>"
```

### Classes as a vec

```clojure
(template/node [:div {:classes ["foo" "bar" "baz"]}])

(.-outerHTML *1)

=> "<div class=\"foo bar baz\"></div>"
```

### Inline style as a map

```clojure
(template/node
  [:span
    {:style
      {:color "#aaa"
       :text-decoration "line-through"}}])

(.-outerHTML *1)

=> "<span style=\"color:#aaa; text-decoration:line-through;\"></span>"
```


### Extensible

Thanks to [@ibdknox](https://github.com/ibdknox/) you can have custom view logic for your <code>deftype</code> or <code>defrecord</code> by implementing the <code>PElement</code> protocol:

```clojure
(defrecord MyModel [data]
   dommy.template/PElement
   (-elem [this] [:p (str "My data " data)]))

(dommy.template/node (MyModel. "is big"))
=> "<p>My data is big</p>"
```

### Document fragments

You can also make document fragments using the slightly more general `template/->node-like`:

```clojure
(template/->node-like
  (list :.class1 :.class2 :.class3))
```
Yields a document fragment for each of the templates in the list. So specifically,

```clojure
(is= "<div><div class="class1"></div><div class="class2"></div><div class="class3"></div></div>"
   (.-outerHTML
    (doto (template/node [:div])
      (.appendChild 
       (template/->node-like (list :.class1 :.class2 :.class3))))))
```

`template/->node-like` only returns a document fragment when passed a list. On a vector it attempts
to coerce into a single compound element as with `template/node`.

### Compile Macros

There is a also a macro DOM building function which can be significantly faster if most of your template structure can be expressed nested vector literals. It's really worth taking a look at the code and understanding what will and won't be done at compile time (see ns <code>dommy.macros</code>). Here's an example:

```clojure
(defn simple-template [word]
  (dommy.macros/node
    [:div#id.class1
	  [:span.text word]]))
```

Alternatively, you can use the <code>deftemplate</code> macro:


```clojure
(deftemplate simple-template [word]
  [:div#id.class1
	  [:span.text word]])
```

These macros will compile this template data into **much** more efficient JavaScript

```javascript
function simple_template(word) {
 var dom56633 = document.createElement("div");
  dom56633.className = "class1";
  dom56633.setAttribute("id", "id");
  dom56633.appendChild(function() {
    var dom56634 = document.createElement("span");
    dom56634.className = "text";
    dom56634.appendChild(dommy.template._elem.call(null, word));
    return dom56634
  }());
  return dom56633
}
```

The <code>node</code> macro will 'compile' the structure to efficient JavaScript recursively as
long as data is expressed as literals. It falls back to the runtime macro when it encounters a variable
or some 'unparseable' structure. 

### Type-Hinting Template Macros

One caveat of using the compile-macro is that if you have a compound element (a vector element) and want to have a non-literal map as the attributes (the second element of the vector), then you need to use <code>^:attrs</code> meta-data so the compiler knows to process this symbol as a map of attributes in the runtime system. Here's an example


```clojure
(dommy.macros/node  [:a ^:attrs (merge m1 m2)])
```

Again this is **not** necessary when the attribute map is a literal (that map can even contain symbolic keys or values).

You can also type-hint a symbol as <code>^:text</code> which will ensure the macro appends
the symbol as a text node and doesn't use the runtime templating.

For instance, this template

```clojure
(deftemplate simple-template [[href anchor]]
    [:a.anchor {:href href} ^:text anchor])
```

Will generate the following Javascript:


```javascript
function simple_template(p__13888) {
    var vec__13891 = p__13888;
    var href = cljs.core.nth.call(null, vec__13891, 0, null);
    var anchor = cljs.core.nth.call(null, vec__13891, 1, null);
    var dom13892 = document.createElement("a");
    dom13892.className = "anchor";
    if(cljs.core.truth_(href)) {
        dom13892.setAttribute("href", href)
    }else {
    }
    dom13892.appendChild(document.createTextNode(anchor));
    return dom13892
}
```

## Dom Manipulation

Dommy also has DOM manipulation similar to jQuery, which can be threaded using the `->` operator
to simulate jQuery-style chaining.

```clojure
(= (-> [:#root.container]
       (dommy/append! (dommy/add-class! [:span "Some Text"] "child-class"))
  	   (dommy/toggle-class! "container")
       .-outerHTML)
  "<div id=\"id\"><span class=\"child-class\">Some Text</span></div>")
  
;; Also equivalent to the simpler template
(dommy.macros/node [:div#root [:span.child-class "Some Text"]])  
```

According to a simple benchmark, dommy performs twice as fast compared to jQuery on a simple `toggleClass`, `hasClass` benchmark.

## Testing

Dommy comes with reasonably extensive tests. To run them you must build the `test`  cljsbuild target and open
up the HTML file under `resources/dommy-tests.html` which will give you a visual representation of all tests. For
all pull requests, please ensure your tests pass (or add test cases) before submitting. 


## License

Copyright (C) 2013 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
