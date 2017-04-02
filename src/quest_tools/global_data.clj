(ns quest-tools.global-data
  (:require
    [clojure.java.io :as io]
    [cheshire.core :as cheshire]))

(defn skills-map-from-json-file [filename]
  (let [raw-data
        (first
          (cheshire/parsed-seq (io/reader filename)))]
    (->>
      raw-data
      (mapcat (fn [v] [(get v "key") v]))
      (apply hash-map))))
