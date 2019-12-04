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
  "Given the attribute with scaling returns nominal scaled incidence with
   attributes."
  [attribute scaling]
  (let [distinct-values (:distinct scaling)
        base-map        (build-base distinct-values)]
    ;; build ((attributes)((incidence)))
    (list ;; scaled attributes are named "original | value"
          (map #(str attribute "|" %) distinct-values)
          ;; get the incidence row for each value
          (list
            (map
              #(get base-map %)
              (:values scaling))))))

;;;-Scaling--------------------------------------------------------------------

(defn apply-measure
  "Selects the appropriate function for the given attribute."
  [attribute scaling]
  (case (:measure scaling)
    "nominal" (scale-nominal attribute scaling)
    (scale-nominal attribute scaling)))

(defn scale 
  "Applies selected scales to all attributes."
  [db]
  (let [;; attributes which will be scaled
        attributes (get-in db [:selection :attributes])
        ;; and how they are scaled
        scaling    (get-in db [:scaling])] 
    (map 
      #(apply-measure % ((keyword %) scaling))
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
