(ns quest-tools.character
  (:require
    [clojure.java.io :as io]
    [cheshire.core :as cheshire]
    [quest-tools.global-data :as gd]
    [quest-tools.bbcode.utils :as bbcode]))

(def default-affinities
  {:wind -10
   :earth -10
   :lightning -10
   :fire -10
   :water -10})

(defonce
  attrs-in-order
  [{:key "str"
    :label "Strength"}
   {:key "agi"
    :label "Agility"}
   {:key "con"
    :label "Constitution"}
   {:key "chk"
    :label "Chakra Capacity"}
   {:key "per"
    :label "Perception"}
   {:key "int"
    :label "Intelligence"}])
(defonce attr-order (map :key attrs-in-order))
(defonce
  attr-key-to-label
  (->>
    attrs-in-order
    (mapcat (fn [t] [(:key t) (:label t)]))
    (apply hash-map)))

(defn default-attrs []
  (->>
    attr-order
    (mapcat (fn [a] [a {"base" 10 "mod" 0}]))
    (apply hash-map)))

(defn from-json-file [filename]
  (let [raw-data
        (first
          (cheshire/parsed-seq (io/reader filename)))]
    raw-data))

(defn map-attrs-ordered
  "Prints the attributes for a character in the default ordering."
  [f attrs]
  (->>
    attr-order
    (map (fn [a] [a (get attrs a)]))
    (remove #(nil? (second %)))
    (map #(apply f %))))

(defonce attr-xp-basis 16)
(defn attr-xp-for-next-rank [attr-mod character]
  (int
    (*
     (or
      (get character "attr_xp_mult") 1)
     (+ attr-mod attr-xp-basis))))

(defn tech-xp-for-next-rank [rank basis character]
  (let [rank-adjustment
        (int (/ rank 2))]
    (max 1
      (int
        (*
         (or
          (get character "tech_xp_mult") 1)
         (+ rank-adjustment basis))))))

(defn skill-xp-for-next-rank [rank basis character]
  (let [rank-adjustment
        (int
          (/
           (max 0 (- rank 10))
           2))]
    (max 1
      (int
        (*
         (or
          (get character "skill_xp_mult") 1)
         (+ rank-adjustment basis))))))

(defonce
  tech-penalty-by-rank
  {"s" -16
   "a" -12
   "b" -8
   "c" -4
   "d" 0
   "e" 0})

(defn letter-for-attr-rank [rank]
  (cond
    (>= 8 rank) "E-"
    (>= 9 rank) "E"
    (>= 10 rank) "E+"
    (>= 11 rank) "D-"
    (>= 12 rank) "D"
    (>= 13 rank) "D+"
    (>= 14 rank) "C-"
    (>= 15 rank) "C"
    (>= 16 rank) "C+"
    (>= 17 rank) "B-"
    (>= 18 rank) "B"
    (>= 19 rank) "B+"
    (>= 20 rank) "A-"
    (>= 21 rank) "A"
    (>= 22 rank) "A+"
    (>= 23 rank) "S-"
    (>= 24 rank) "S"
    :else "S+"))

(defn letter-for-skill-rank [rank]
  (cond
    (>= 4 rank) "E-"
    (>= 5 rank) "E"
    (>= 7 rank) "E+"
    (>= 8 rank) "D-"
    (>= 10 rank) "D"
    (>= 11 rank) "D+"
    (>= 12 rank) "C-"
    (>= 14 rank) "C"
    (>= 15 rank) "C+"
    (>= 16 rank) "B-"
    (>= 18 rank) "B"
    (>= 19 rank) "B+"
    (>= 20 rank) "A-"
    (>= 22 rank) "A"
    (>= 23 rank) "A+"
    (>= 24 rank) "S-"
    (>= 26 rank) "S"
    :else "S+"))

(defn word-for-tech-rank [rank]
  (cond
    (> 6 rank) "hopeless"
    (> 10 rank) "poor" ; 6 7 8 9
    (> 12 rank) "unreliable" ; 10 11
    (> 14 rank) "acceptable" ; 12 13
    (> 16 rank) "practiced" ; 14 15
    (> 20 rank) "instinctive" ; 16 17 18 19
    :else "masterful"))

; TODO: calculate nature/element affinity penalty
(defn default-for-tech [char-tech character techniques-map]
  (let [char-skills-map
        (->>
          (get character "skills")
          (mapcat (fn [v] [(get v "key") v]))
          (apply hash-map))

        tech-id (get char-tech "key")
        tech (get techniques-map tech-id)

        worst-char-skill-key
        (->>
          (get tech "parent_skills")
          (reduce
            (fn [a b]
              (if
                (<
                  (get (get char-skills-map a) "rank")
                  (get (get char-skills-map b) "rank"))
                a b))))

        worst-char-skill-rank
        (get
          (get char-skills-map worst-char-skill-key)
          "rank")]
    (+ worst-char-skill-rank
       (get tech-penalty-by-rank
            (get tech "difficulty_rank")))))

(defn bbcode-attr-line [attr char-attr character]
  (let [attr-total
        (+
          (get char-attr "mod")
          (get char-attr "base"))]
    (str
      (bbcode/b
        (get attr-key-to-label attr))
      " "
      (bbcode/color-grey
        (get char-attr "base")
        "+"
        (get char-attr "mod")
        "=")
      (bbcode/b attr-total)
      " (" (letter-for-attr-rank attr-total) ") "
      (bbcode/color-grey
        " ["
        (get char-attr "xp")
        "/"
        (attr-xp-for-next-rank
          (get char-attr "mod")
          character)
        " xp]"))))

(defn bbcode-skill-line [char-skill character skills-map]
  (let [skill-id (get char-skill "key")
        skill (get skills-map skill-id)
        skill-rank (get char-skill "rank")]
    (str
      (bbcode/b (get skill "name"))
      " "
      skill-rank
      " (" (letter-for-skill-rank skill-rank) ") "
      (bbcode/color-grey
        " ["
        (get char-skill "xp")
        "/"
        (skill-xp-for-next-rank
          skill-rank
          (get skill "xp_basis")
          character)
        " xp]"))))

(defn bbcode-tech-block [char-tech character techniques-map]
  (let [tech-id (get char-tech "key")
        tech (get techniques-map tech-id)
        tech-rank (get char-tech "rank")
        skill-default
        (default-for-tech
          char-tech character techniques-map)
        adjusted-tech-rank
        (+ tech-rank skill-default)]
    (str
      (bbcode/b (get tech "name"))
      " "
      (bbcode/color-grey
        skill-default
        "+"
        tech-rank
        "=")
      adjusted-tech-rank
      " (" (word-for-tech-rank adjusted-tech-rank) ") "
      (bbcode/color-grey
        " ["
        (get char-tech "xp")
        "/"
        (tech-xp-for-next-rank
          tech-rank
          (get tech "xp_basis")
          character)
        " xp]")
      "\n"
      (bbcode/color-grey
        "("
        (get tech "range")
        "; "
        (get tech "difficulty_rank")
        "-rank) "
        (bbcode/i (get tech "desc")))
      )))

(defn bbcode-trait-block [trait]
  (str
    (bbcode/b (get trait "name"))
    "\n"
    (bbcode/i (get trait "desc"))))

(defn to-bbcode [character skills-map techniques-map]
  (str
    (get character "name")
    "\n"
    (get character "fluff")
    "\n\n"
    (when-let [unused-xp (get character "unused_xp")]
      (str
        (bbcode/b "Unused XP: ")
        unused-xp
        "\n\n"))
    (bbcode/b "Nature Affinities")
    (bbcode/article
      ""
      (clojure.string/join
        ", "
        (map
          #(clojure.string/join " " %)
          (get character "affinities"))))
    (bbcode/b "Attributes")
    (bbcode/article
      ""
      (->>
        (get character "attributes")
        (map-attrs-ordered
          #(bbcode-attr-line %1 %2 character))
        (clojure.string/join "\n")))
    (bbcode/b "Skills")
    (bbcode/article
      ""
      (->>
        (get character "skills")
        (map #(bbcode-skill-line
                % character skills-map))
        (clojure.string/join "\n")))
    (bbcode/b "Techniques")
    (bbcode/article
      ""
      (->>
        (get character "techniques")
        (map
          #(bbcode-tech-block
             % character techniques-map))
        (clojure.string/join "\n")))
    (bbcode/b "Traits")
    (bbcode/article
      ""
      (->>
        (get character "traits")
        (map bbcode-trait-block)
        (clojure.string/join "\n\n")))))
