(ns online-scaler.scaling
  (:require [clojure.string :as string]))

;;;-Nominal-Scale--------------------------------------------------------------

(defn build-base
  "Given all distinct values builds a map with their corresponding incidence."
  [values]
  (let [value-count (count values)
        ;; quadratic diagonal matrix with n = value-count
        incidence   (map
                      (fn [row position] (assoc row position "X"))
                      (repeat value-count (vec (repeat value-count ".")))
                      (range value-count))]
    ;; build map with {:value incidence-row}
    (apply merge
      (map
        (fn [value row] (hash-map value row))
        values
        incidence))))

(defn scale-nominal
  "Given all values of a colum scales them nominal."
  [values attribute db]
  (let [new-attributes  (distinct values)
        base-map        (build-base new-attributes)]
    ;; build ((attributes)((incidence)))
    (list (map #(str (first attribute) "|" %) new-attributes)
          ;; get the incidence row for each value
          (list
            (map
              #(get base-map %)
              values)))))

;;;-Scaling--------------------------------------------------------------------

(defn apply-measure
  "Selects the appropriate function for the given attribute."
  [values attribute db]
  (case (get-in db [:scaling (keyword attribute) :measure])
    "nominal" (scale-nominal values attribute db)
    (scale-nominal values attribute db)))

(defn scale 
  "Applies selected scales to all attributes."
  [db]
  (let [;; attributes which will be scaled
        attributes (get-in db [:mv-ctx :attributes])
        mv-ctx     (get-in db [:mv-ctx :file])] 
    (map 
      ;; scale each attribute with its entry in db->scaling
      (fn [values attribute]
        (if (second attribute)
          (apply-measure values attribute db)
          nil))
      mv-ctx
      attributes)))

(defn write 
  "Converts the scaled context into burmeister format."
  [db]
  (let [;; combine list of ((atts)((inc))) into one ((atts)(incs))
        context   (apply map concat (filter some? (scale db)))
        attributes (first context)
        ;; combine the incidence lists
        incidence  (apply map concat (second context))
        objects    (range (count incidence))]
    (str \B \newline
         \newline
         (count objects) \newline
         (count attributes) \newline
         \newline
         (string/join \newline objects) \newline
         (string/join \newline attributes) \newline
         (string/join \newline (map #(apply str %) incidence)))))
