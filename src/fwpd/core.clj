(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int [str] (Integer. str))

(def conversions {:name identity
                :glitter-index str->int})

(defn convert
    [vamp-key value]
    ((get conversions vamp-key) value))

(defn parse
    [string]
    (map #(clojure.string/split % #",")
        (clojure.string/split string #"\n")))

(defn vamp-row->map
    [row]
    (reduce 
        (fn [memo [key val]]
            (assoc memo key (convert key val)))
        {}
        (map vector vamp-keys row)))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map 
    vamp-row->map
    rows))

(defn glitter-filter
    [min vamps]
    (filter
        #(>= (:glitter-index %) min)
        vamps))

(defn list-vamp-names
    [vamps]
    (map #(:name %) vamps))
                
(def validators {:name (complement #(nil? %))
                :glitter-index (complement #(nil? %))})

(defn validate
    [validators vamp]
    (every?
        (fn [[key validator]] (validator (key vamp)))
        validators))

(defn append
    [vamp vamps]
    (when (validate validators vamp)
        (conj vamps vamp)))

(def vamps (mapify (parse (slurp filename))))

(def new-vamp {:name 'vampey' :glitter-index 5})

(def invalid-vamp {:name "bad vamp"})

(defn vamps-to-csv
    [vamps]
    (clojure.string/join
        "\n"
        (map
            (fn [vamp] (str (:name vamp) "," (:glitter-index vamp)))
            vamps)))