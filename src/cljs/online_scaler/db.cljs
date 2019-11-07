(ns online-scaler.db)

(def default-db
  {:panel   "upload"
   :mv-ctx  {:name        [:font {:color "lightgrey"} "No file selected."]
             ;; csv file split up by lines and commas
             :file        nil
             :header      false
             ;; [["att#1" true] ["att#2" false]] only scale true
             :attributes  nil}
   :url     nil})
