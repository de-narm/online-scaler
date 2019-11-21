(ns online-scaler.db)

;;;-Default-Database-----------------------------------------------------------

(def default-db
  {
   ;;;-Import------------------------------------------------------------------
   :db {:name  [:font {:color "lightgrey"} "No file selected."]
        :map   nil ;db to replace current one on button press
        }

   ;;;-Upload/Selection--------------------------------------------------------
   :mv-ctx  {:name        [:font {:color "lightgrey"} "No file selected."]
             :file        nil ;nested list for .csv lines
             :header      false ;does .csv include attribute names?
             :attributes  nil ;list of pairs [attribute, scale?]
             }

   ;;;-Scaling-----------------------------------------------------------------
   :selection {:current-attribute     nil ;what to load in scaling view
               :attributes            nil ;ordered list for next/prev button
               }
   :scaling   nil ;initialized with map of maps
              ;;example element
              ;;:attribute-name {:measure "nominal"}

   ;;;-Export------------------------------------------------------------------
   :warning      false
   :context-url  nil ;downloadlink
   :config-url   nil ;downloadlink
   :scaled-ctx   nil ;ctx described by :scaling
                 ;; example context
                 ;; {:objects    ["obj1" "obj2"]
                 ;;  :attributes ["att1" "att2"]
                 ;;  :incidence  [[true false][false false]]}

   ;;;-Panel/Util--------------------------------------------------------------
   :panel   "import" ;current view
   :tmp     nil ;helper between mouse-down/mouse-up events
   })
