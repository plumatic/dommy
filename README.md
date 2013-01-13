# dommy

Dommy is no-nonsene ClojureScript templating based on Clojure's [Hiccup](https://github.com/weavejester/hiccup/) html templating library. It is similar to [Crate](https://github.com/ibdknox/crate), but instead returns native Javascript DOM elements, rather than HTML and is much faster (3-4x, see the performance comparison test <code>dommy.template-perf-test</code>). 

## Templating Usage

```clojure
(ns awesome-webapp
 (:require [dommy.template :as template]))

(template/node
  [:div#id.class1 
	(for [r (range 2)] 
	 [:span.text (str "word" r)])])
=> [object HTMLElement] 
(.innerHTML *1)
=> "<span class=\"text\">word0</span><span class=\"text\">word1</span>"
```

## License

Copyright (C) 2013 Prismatic

Distributed under the Eclipse Public License, the same as Clojure.
