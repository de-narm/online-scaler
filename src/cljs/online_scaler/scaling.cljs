(ns online-scaler.scaling
  (:require [clojure.string :as string]))

;;;-Nominal-Scale--------------------------------------------------------------

(defn nominal-scale
  "Builds a map with distinct values and their corresponding incidence."
  [scaling]
  (let [values      (:distinct scaling)
        value-count (count values)
        ;; quadratic diagonal matrix with n = value-count
        matrix      (map
                      (fn [row position] (assoc row position "X"))
                      (repeat value-count (vec (repeat value-count ".")))
                      (range value-count))]
    ;; build map with {:value matrix-row}
    (apply merge
      (map
        (fn [value row] (hash-map value row))
        values
        matrix))))

;;;-Ordinal-Scale--------------------------------------------------------------

(defn to-context
  "Given a ordinal sclaing object computes the context out of an order if
   needed."
  [scaling]
  (if (:context-view scaling)
      scaling
      (let [distinct-values   (get-in scaling [:distinct])
            orders            (get-in scaling [:orders])]
        (assoc-in 
          scaling
          [:incidence]
          (loop [incidence {}
                 remaining distinct-values]
            (if (empty? remaining)
                incidence
                (let [value (first remaining)
                      set-true  
                        (apply concat
                          (map
                            (fn [order]
                              (let [elements (:elements order)]
                              (if (some #{value} elements)
                              (case (:relation order)
                                "<"  (take (.indexOf elements value) elements)
                                ">"  (drop (+ 1 (.indexOf elements value))
                                           elements)
                                ">=" (drop (.indexOf elements value) elements)
                                "<=" (take (+ 1 (.indexOf elements value))
                                           elements))
                              (list))))
                            orders))
                      set-false 
                        (apply concat
                          (map
                            (fn [order]
                              (let [elements (:elements order)]
                              (if (some #{value} elements)
                              (case (:relation order)
                                "<"  (drop (.indexOf elements value) elements)
                                ">"  (take (+ 1 (.indexOf elements value))
                                           elements)
                                ">=" (take (.indexOf elements value) elements)
                                "<=" (drop (+ 1 (.indexOf elements value))
                                           elements))
                                (list))))
                            orders))
                       sub-incidence
                        (apply merge
                          (map 
                            #(hash-map (keyword %) true)
                            (clojure.set/difference (set set-true) 
                                                    (set set-false))))]
                  (recur (merge incidence 
                                (hash-map (keyword value) sub-incidence))
                         (drop 1 remaining)))))))))

(defn ordinal-scale
  "Builds a map with distinct values and their corresponding incidence."
  [scaling]
  (let [values     (:distinct scaling)
        attributes (:attributes scaling)
				incidence  (:incidence scaling)
				;; build #valuesX#attributes matrix based on incidence
				matrix  (map
									(fn [value]
										(map 
											(fn [attribute]
												(if (get-in incidence [(keyword attribute)
																							 (keyword value)])
														"X"
														".")) 
											attributes))
									values)]
    ;; build map with {:value matrix-row}
    (apply merge
      (map
        (fn [value row] (hash-map value row))
        values
        matrix))))

;;;-Scaling--------------------------------------------------------------------

(defn select-function
  "Selects the appropriate function for the given data."
  [scaling]
  (case (:measure scaling)
    "nominal" (nominal-scale scaling)
    "ordinal" (ordinal-scale (to-context scaling))
    nil))

(defn apply-measure
  "Applies scaling data to each object."
  [scaling]
  (let [base-map        (select-function scaling)]
    ;; get the incidence row for each value
		(map
			#(get base-map %)
			(:values scaling))))

(defn scale 
  "Applies selected scales to all attributes."
  [attributes scaling]
	(map 
		#(apply-measure ((keyword %) scaling))
		attributes))

;;;-Attributes-----------------------------------------------------------------

(defn get-attributes
  "Get all new attributes based on measure."
  [attributes scaling]
	(map
		#(let [data ((keyword %) scaling)]
			 (map
         (fn [string] (str % "|" string))
         (case (:measure data)
           "nominal" (:distinct data)
           "ordinal" (if (:context-view data) 
                         (:attributes data) 
                         (:distinct data))
           nil)))
		attributes))

;;;-Burmeister-----------------------------------------------------------------

(defn write 
  "Converts the scaled context into burmeister format."
  [db]
  (let [;; attributes which will be scaled
        old-attributes (get-in db [:selection :attributes])
        ;; and how they are scaled
        scaling        (get-in db [:scaling])
				;; get a list of all new attributes
        attributes (apply concat (get-attributes old-attributes scaling))
        ;; combine list of incidences into one
        incidence  (apply map concat (scale old-attributes scaling))
        ;; objects are just numbered
        objects    (range (count incidence))]
    (str \B \newline
         \newline
         (count objects) \newline
         (count attributes) \newline
         \newline
         (string/join \newline objects) \newline
         (string/join \newline attributes) \newline
         (string/join \newline (map #(apply str %) incidence)))))
