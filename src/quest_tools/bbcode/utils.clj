(ns quest-tools.bbcode.utils)

(defn with-tag [tag-name tag-value & content]
  (str
    "["
    tag-name
    (if tag-value (str "=" tag-value))
    "]"
    (let [content (flatten content)]
      (clojure.string/join "" content) )
    "[/" tag-name "]"))

(def b (partial with-tag "b" nil))
(def i (partial with-tag "i" nil))
(def u (partial with-tag "u" nil))

(def spoiler (partial with-tag "spoiler"))
(def article (partial with-tag "article"))

(def color (partial with-tag "color"))
(def color-grey (partial color "#999"))
