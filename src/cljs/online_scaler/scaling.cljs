(ns online-scaler.scaling
  (:require [clojure.string :as string]
            [clojure.set :refer [difference]]))

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
      (mapv
        (fn [value row] (hash-map value row))
        values
        matrix))))

;;;-Ordinal-Scale--------------------------------------------------------------

(defn to-context
  "Given a ordinal scaling object computes the context out of an order if
   needed. The flag decides whether or not empty attributes are kept."
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
                      ;; for the given value get all true implications from
                      ;; the orders
                      set-true  
                        (apply concat
                          (map
                            (fn [order]
                              (let [elements (:elements order)]
                                (if 
                                  (some #{value} elements)
                                  (case (:relation order)
                                    "<"  (take (.indexOf elements value) 
                                               elements)
                                    ">"  (drop (+ 1 (.indexOf elements value))
                                               elements)
                                    ">=" (drop (.indexOf elements value) 
                                               elements)
                                    "<=" (take (+ 1 (.indexOf elements value))
                                               elements))
                                  (list))))
                            orders))
                      ;; and all false implications
                      set-false 
                        (apply concat
                          (map
                            (fn [order]
                              (let [elements (:elements order)]
                                (if 
                                  (some #{value} elements)
                                  (case (:relation order)
                                    "<"  (drop (.indexOf elements value) 
                                               elements)
                                    ">"  (take (+ 1 (.indexOf elements value))
                                               elements)
                                    ">=" (take (.indexOf elements value) 
                                               elements)
                                    "<=" (drop (+ 1 (.indexOf elements value))
                                               elements))
                                    (list))))
                            orders))
                       ;; the difference gets actually marked as true
                       sub-incidence
                        (apply merge
                          (map 
                            #(hash-map (keyword %) true)
                            (difference (set set-true) 
                                        (set set-false))))]
                  (recur (merge incidence 
                           (hash-map (keyword value) sub-incidence))
                         (drop 1 remaining)))))))))

(defn ordinal-scale
  "Builds a map with distinct values and their corresponding incidence."
  [scaling]
  (let [values     (:distinct scaling)
        attributes (if (:context-view scaling)
                     (:attributes scaling)
                     (distinct 
                       (apply concat (map :elements (:orders scaling)))))
				incidence  (:incidence scaling)
				;; build #valuesX#attributes matrix based on incidence
				matrix  (mapv
									(fn [value]
										(mapv 
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

;;;-Numeric-Scale--------------------------------------------------------------

(defn numeric-scale
  "Builds a map with distinct values and their corresponding incidence."
  [scaling]
  (let [values     (:distinct scaling)
        attributes (filter identity (:selected scaling))
        matrix     (map
                     (fn [value]
                       (map 
                         (fn [attribute] 
                           (if  (every? identity 
                                  (map 
                                    (fn [interval] 
                                       (and 
                                         ((if (= "[" (:left interval)) >= >) 
                                            value 
                                            (:start interval)) 
                                         ((if (= "]" (:right interval)) <= <)
                                            value
                                            (:end interval))))
                                    (filter identity (:intervals attribute))))
                               "X"
                               "."))
                       attributes))
                     values)]
    ;; build map with {:value matrix-row}
    (apply merge
      (mapv
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
    "numeric" (numeric-scale scaling)
    nil))

(defn apply-measure
  "Applies scaling data to each object."
  [scaling]
  (let [base-map (select-function scaling)]
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
                         (distinct 
                           (apply concat (map :elements (:orders data)))))
           "numeric" (map :name 
                          (filter identity 
                                  (:selected data)))
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
        objects    (get-in db [:ctx :objects])]
    (str \B \newline
         \newline
         (count objects) \newline
         (count attributes) \newline
         \newline
         (string/join \newline objects) \newline
         (string/join \newline attributes) \newline
         (string/join \newline (map #(apply str %) incidence)))))
