(ns online-scaler.db)

;;;-Default-Database-----------------------------------------------------------

(def default-db
  {;;;-Import------------------------------------------------------------------
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
              ;; {:attribute-name  {:measure   "nominal"
              ;;                    :numerical false  ;true == dom only numbers
              ;;                    :values    [vals] ;ordered values 
              ;;                    :distinct  [vals] ;distinct values
              ;;                    }
              ;;  :other-attribute {:measure      "ordinal"
              ;;                    :numerical    false
              ;;                    :values       [vals]
              ;;                    :distinct     [vals]
              ;;                    :context-view false
              ;;                    ---------Drag------------------------------
              ;;                    :orders [{:relation "<"
              ;;                              :pos      int ;pos on array
              ;;                              :elements [vals]}]
              ;;                    ---------Context---------------------------
              ;;                    :attributes    [atts]
              ;;                    :incidence     {:att {:obj true}}
              ;;                    :relation-name "name" ;given in ctx view
              ;;                    }
              ;;  :other-attribute {:measure      "numerical"
              ;;                    :numerical    true
              ;;                    :values       [vals]
              ;;                    :distinct     [vals]
              ;;                    :context-view false
              ;;                    ---------Numeric---------------------------
              ;;                    :selected [{:name      "name"
              ;;                                :pos       int 
              ;;                                :intervals [{:pos   int
              ;;                                             :left  "["
              ;;                                             :start num
              ;;                                             :end   num
              ;;                                             :right ")"}]}]
              ;;                    :generated [{:name      "name"
              ;;                                 :pos       int
              ;;                                 :intervals [{:pos   int
              ;;                                              :left  "["
              ;;                                              :start num
              ;;                                              :end   num
              ;;                                              :right ")"}]}]
              ;;                    }
   ;;;-Export------------------------------------------------------------------
   :warning      false
   :context-url  nil ;downloadlink
   :config-url   nil ;downloadlink

   ;;;-Panel/Util--------------------------------------------------------------
   :panel   "import" ;current view
   :tmp     nil ;helper between mouse-down/mouse-up events
   })
