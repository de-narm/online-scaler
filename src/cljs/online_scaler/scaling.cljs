(ns online-scaler.scaling)

;;;-Nominal-Scale--------------------------------------------------------------

(defn scale-nominal
  "Get all Elements and use them as new attributes."
  [values attribute db]
  (let [header      (get-in db [:mv-ctx :header])
        mv-ctx          (get-in db [:mv-ctx :file])
        new-attributes  (distinct values)
        empty-incidence (repeat 
                          (if header (count (drop 1 mv-ctx)) (count mv-ctx))
                          (vec (repeat (count new-attributes) ".")))]
    {:attributes (map #(str (first attribute) "|" %) new-attributes)
     :incidence  (map
                   (fn [a b]
                     (assoc a (.indexOf new-attributes b) "X")) 
                   empty-incidence
                   values)}))

;;;-Scaling--------------------------------------------------------------------

(defn apply-measure
  "Selects the appropriate function for the given attribute."
  [values attribute db]
  (case (get-in db [:scaling (keyword attribute) :measure])
    "Nominal" (scale-nominal values attribute db)
    (scale-nominal values attribute db)))

(defn scale 
  "Applies selected scales to all attributes."
  [db]
  (let [;; attributes which will be scaled
        attributes (get-in db [:mv-ctx :attributes])
        header     (get-in db [:mv-ctx :header])
        mv-ctx     (get-in db [:mv-ctx :file])] 
    (map 
      (fn [values attribute]
        (if (second attribute)
          (apply-measure values attribute db)
          nil))
      ;; [[1 1][2 2]] -> [[1 2][1 2]]
      (apply map list (if header (drop 1 mv-ctx) mv-ctx))
      attributes)))

(defn write 
  "Converts the scaled context into burmeister format."
  [db]
  (let [contexts   (scale db)
        objects    (range 
                     (if (get-in db [:mv-ctx :header])
                       (- (count (get-in db [:mv-ctx :file])) 1)
                       (count (get-in db [:mv-ctx :file]))))
        attributes (apply concat (map :attributes contexts))
        incidence  (apply map concat (filter some? (map :incidence contexts)))]
    (str "B\n"
         "\n"
         (count objects) "\n"
         (count attributes) "\n"
         "\n"
         (apply str (map #(str % "\n") objects)) 
         (apply str (map #(str % "\n") attributes)) 
         (apply str (map #(str (apply str %) "\n") incidence)))))
