(ns online-scaler.db)

(def default-db
  {;; the current panel to be loaded from view
   :panel   "upload"
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
               :attributes            nil
               :subpanel              "attribute"}
   ;; map of maps with scaling data for each attribute
   :scaling nil
     ;; example map
     ;{:measure  "nominal"}
   })
