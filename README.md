# dommy

Dommy is no-nonsene ClojureScript templating based on Clojure's [Hiccup](https://github.com/weavejester/hiccup/) html templating library. It is similar to [Crate](https://github.com/ibdknox/crate), but is much faster (3-4x, see the performance comparison test <code>dommy.template-perf-test</code>). 

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

## License

Copyright (C) 2013 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
