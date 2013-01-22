# dommy

Dommy is no-nonsene ClojureScript templating based on Clojure's [Hiccup](https://github.com/weavejester/hiccup/) html templating library. It is similar to [Crate](https://github.com/ibdknox/crate), but is much faster (3-4x, see the performance comparison test <code>dommy.template-perf-test</code>). It also has a compile-time macro component that is significantly (5x) faster, but requires most of you DOM structure to be expressed as nested vector literals (see 'Compile Macros' below).

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

### Classes as a vec/sec

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

### Compile Macros (Experimental)

There is a also a macro DOM building function which can be significantly faster if most of your template structure can be expressed nested vector literals. It's really worth taking a look at the code and understanding what will and won't be done at compile time (see ns <code>dommy.template-compile</code>). Here's an example:

```clojure

(defn simple-template [word]
  (dommy.template-compile/node
    [:div#id.class1
	  [:span.text word]]))
```

The <code>dommy.template-compile/node</code> will compile this template data into **much** more efficient
JavaScript

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
long as data is expressed as literals. 

One caveat of using the compile-macro is that if you have a compound element (a vector element) and want to have a non-literal map as the attributes (the second element of the vector), then you need to use <code>^:attrs</code> meta-data so the compiler knows to process this symbol as a map of attributes in the runtime system. Here's an example


```clojure
(dommy.template-compile/node  [:a ^:attrs (merge m1 m2)])
```

Again this is **not** necessary when the attribute map is a literal (that map can even contain symbolic keys or values).
  

## License

Copyright (C) 2013 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
