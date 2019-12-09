(ns online-scaler.db)

;;;-Default-Database-----------------------------------------------------------

(def default-db
  {
   ;;;-Import------------------------------------------------------------------
   :db {:name  nil ;file name
        :map   nil ;db to replace current one on button press
        }

   ;;;-Upload/Selection--------------------------------------------------------
   ;; upon selection the file is written here
   :mv-ctx  {:name        nil ;file name
             :file        nil ;nested list for .csv lines
             :header      false ;does .csv include attribute names?
             }
   ;; once confirmed :ctx/:scaling get build and :mv-ctx becomes nil
   :ctx     {:name       nil
             :attributes  nil ;list of pairs [attribute, scale?]
             }

   ;;;-Scaling-----------------------------------------------------------------
   :selection {:current-attribute     nil ;what to load in scaling view
               :attributes            nil ;ordered list for next/prev button
               }
   :scaling   nil ;initialized with map of maps
              ;; example element
              ;; {:attribute-name  {:measure "nominal"
              ;;                    :values    [vals] ;ordered values 
              ;;                    :distinct  [vals] ;distinct values
              ;;                    }
              ;;  :other-attribute {:measure "ordinal"
              ;;                    :values     [vals]
              ;;                    :distinct  [vals]
              ;;                    :attributes [atts]
              ;;                    :incidence {:att {:obj true}}
              ;;                    :relation-name "name" ;given in ctx view
              ;;                    }

   ;;;-Export------------------------------------------------------------------
   :warning      false
   :context-url  nil ;downloadlink
   :config-url   nil ;downloadlink

   ;;;-Panel/Util--------------------------------------------------------------
   :panel   "import" ;current view
   :tmp     nil ;helper between mouse-down/mouse-up events
   })
