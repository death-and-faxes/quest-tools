(ns quest-tools.core
  (:require
    [quest-tools.character :as character]
    [quest-tools.global-data :as gd]
    ))

(defn main []
  (let [skills-map
        (gd/skills-map-from-json-file "data/skills/skills.json")
        techniques-map
        (gd/skills-map-from-json-file "data/skills/techniques.json")]
    (->
      ;"data/chars/kana.json"
      "data/chars/ikki.json"
      character/from-json-file
      (character/to-bbcode skills-map techniques-map)
      println)))

;(main)
