(ns online-scaler.db)

;;;-Default-Database-----------------------------------------------------------

(def default-db
  {;; the current panel to be loaded from view
   :panel   "import"
   :mv-ctx  {:name        [:font {:color "lightgrey"} "No file selected."]
             ;; csv file split up by lines and commas
             :file        nil
             :header      false
             ;; [["att#1" true] ["att#2" false]]; only scale true
             :attributes  nil}
   ;; the url to the scaled context for the download button
   :url     nil
   :selection {;; the selected element for the "Scaling" view
               :current-attribute     nil
               ;; vector with selected attributes
               :attributes            nil}
   ;; map of maps with scaling data for each attribute
   :scaling nil
     ;; example map
     ;{attribute {:measure  "nominal"}}
   ;; holds all kinds of temporary values e.g. between mouse-down and mouse-up
   :tmp     nil
   })
