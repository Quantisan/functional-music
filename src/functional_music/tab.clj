(ns functional-music.tab
  (:require [clojure.string :as s]))

(defn filter-tab-lines
  "Only keep the lines that are tabs. Assuming that tab lines must have a '-' and '|' characters."
  [coll]
  (->> coll
       ;; filter lines with '-' and '|' characters
       (filter #(.contains % "-"))
       (filter #(.contains % "|"))))

(defn- join-tabs
  "Join two sets of tabs together"
  [x y]
  (map (comp #(apply str %) concat) x y))

(defn continuous-tab
  "Group all the tabs into one long piece"
  [coll]
  (->> coll
       (partition 6)
       (reduce join-tabs)))

(defn remove-format [coll]
  (->> coll
       (map #(s/replace % #"\|" ""))
       (map #(s/replace % #"^[A-z]" ""))))

(defn to-string-fret [[s f]]
  [s (java.lang.Character/getNumericValue f)])

(defn tab->string-fret [e B G D A E]
  (->> [E A D G B e]
       (map-indexed vector)
       ;; coerce char to string then check with regex
       (filter #(re-seq #"[0-9]" (str (second %))))
       (map to-string-fret)))

(defn parse-guitar-tab [s]
  (->> s
       (s/split-lines)
       (filter-tab-lines)
       (remove-format)
       (continuous-tab)
       (apply map tab->string-fret)))

