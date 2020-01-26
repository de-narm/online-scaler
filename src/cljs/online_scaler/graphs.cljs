(ns online-scaler.graphs)

;;;-Bar------------------------------------------------------------------------

(defn bar [values]
  {:data {:values (for [[k v] (frequencies values)] 
                       (hash-map :a (name k) :b v))}
   :mark "bar"
   :encoding {:y {:field "a"
                  :type "ordinal"
									:axis {:title "Elements"}}
              :x {:aggregate "sum"
                  :field "b"
                  :type "quantitative"
									:axis {:title "Frequency"}}}})

;;;-Density--------------------------------------------------------------------

(defn density [values]
  {:data {:values (for [[k v] (frequencies values)] 
                       (hash-map :a (name k) :density (/ v (count values))))}
	 :width 600
	 :height 200
	 :transform [:density "Values"
               :bandwith 0.3]
   :mark "area" 
   :encoding {
     :x {
       :field "a"
       :title "Values"
       :type "quantitative"
     }
     :y {
       :field "density"
       :type "quantitative"}}})

