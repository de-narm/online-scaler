(ns online-scaler.views
  (:require
   [clojure.string :refer [capitalize split]]
   [re-frame.core :as re-frame]
   [online-scaler.view-util :as util]
   [online-scaler.events :as events]
   [online-scaler.subs :as subs]
   [goog.string :as gstring]))

;;;-Header---------------------------------------------------------------------

(defn header [current-panel]
  (let [can-select @(re-frame/subscribe [::subs/ctx-attributes])
        can-scale @(re-frame/subscribe [::subs/current-attribute])]
    [:div {:class "tabs is-centered"}
      [:ul
        (map
          (fn [a b] 
            (if 
              (not (nil? a))
              (vector :li (merge 
                            {:key a}
                            (if (= a current-panel) {:class "is-active"} {}))
                       [:a {:on-click #(re-frame/dispatch
                                        b)}
                         (capitalize a)])))
          (vector "import" 
                  (if (not (nil? can-select)) "select")
                  (if (not (nil? can-scale)) "scale")
                  (if (not (nil? can-scale)) "export"))
          (vector [::events/set-panel "import"]
                  [::events/set-panel "select"]
                  [::events/initiate-attributes nil]
                  [::events/set-export-warning nil]))]]))

;;;-Upload---------------------------------------------------------------------

(defn upload-file []
  (let [file-name @(re-frame/subscribe [::subs/mv-ctx-file-name])]
    [:div {:class "file has-name is-boxed is-info is-pulled-right"}
      [:label {:class "file-label"}
        [:input {:class "file-input" 
                 :type "file"
                 :accept ".csv"
                 :on-change #(re-frame/dispatch 
                              [::events/mv-ctx-file-change 
                                (-> % .-target .-files (aget 0))])}]
        [:span {:class "file-cta"}
          [:span {:class "file-icon"}
            [:i {:class "fas fa-upload"}]]
          [:span {:class "file-label"}
            "Choose a file…"]]
        [:span {:class "file-name"}
          (if (nil? file-name) 
              [:font {:color "lightgrey"} "No file selected."]
              file-name)]]]))

(defn upload-checkbox []
	[:label {:class "checkbox is-pulled-left"}
		[:input {:type "checkbox"
             :checked @(re-frame/subscribe [::subs/checkbox])
             :on-change #(re-frame/dispatch
                          [::events/mv-ctx-header-change
                            (-> % .-target .-checked)])}]
		        "Header?"])

(defn upload-form []
  (let [file (re-frame/subscribe [::subs/mv-ctx-file])]
    [:div {:class "columns"} 
      [:div {:class "column"}
        [upload-file]]
      [:div {:class "column"}
        [upload-checkbox]
        [:br]
        [:br]
        (if (not (nil? @file))
          [:button {:type "button"
                    :class "button is-large is-info is-pulled-left"
                    :on-click #(re-frame/dispatch
                               [::events/initiate-selection nil])}
                   "Upload"])]]))

;;;-Import---------------------------------------------------------------------

(defn import-file []
  (let [import-name @(re-frame/subscribe [::subs/import-name])]
    [:div {:class "file has-name is-boxed is-info is-pulled-right"}
      [:label {:class "file-label"}
        [:input {:class "file-input" 
                 :type "file"
                 :accept ".json"
                 :on-change #(re-frame/dispatch 
                              [::events/import-file-change 
                                (-> % .-target .-files (aget 0))])}]
        [:span {:class "file-cta"}
          [:span {:class "file-icon"}
            [:i {:class "fas fa-upload"}]]
          [:span {:class "file-label"}
            "Choose a file…"]]
        [:span {:class "file-name"}
          (if (nil? import-name)
              [:font {:color "lightgrey"} "No file selected."]
              import-name)]]]))

(defn import-form []
  (let [file (re-frame/subscribe [::subs/import-db])]
    [:div {:class "columns"} 
      [:div {:class "column"}
        [import-file]]
      [:div {:class "column"}
        (if (not (nil? @file))
          [:button {:type "button"
                    :class "button is-large is-info is-pulled-left"
                    :on-click #(re-frame/dispatch
                               [::events/set-db nil])}
                   "Import"])]]))

(defn import-panel []
  [:div {:class "container is-fluid"}
		[:div {:class "container box has-text-centered"}
      [:h1 {:class "title"} "Upload Many Valued Context"]
      [upload-form]]
		[:div {:class "container box has-text-centered"}
      [:h1 {:class "title"} "Import Config File"]
      [import-form]]])

;;;-Select---------------------------------------------------------------------

(defn select-preview []
  (let [attributes (map 
                     first
                     @(re-frame/subscribe [::subs/ctx-attributes]))
        ;; get the first 5 elements from each attribute; transpose the matrix
        values (apply map list
                 (map #(take 4 @(re-frame/subscribe [::subs/ctx-values %])) 
                      attributes))]
    [:div {:class "box"}
      [:div {:class "level is-marginless"}
        [:h1 {:class "title"} "Preview"]
        [util/tooltip "Try holding shift to scroll sideways!"]]
      [:div {:class "table-container is-unselectable"}
        [:table {:class "table is-bordered"}
          [:thead 
            [:tr (doall (map #(vector :th {:key %} 
                                [:div 
                                  util/abbreviate-text
                                  [:span {:title %} %]]) 
                             attributes))]]
          [:tbody                     
            (doall
              (map (fn [row rnum] (vector 
                                    :tr {:key rnum}
                                    (map 
                                      (fn [element cnum] 
                                        (vector 
                                          :td 
                                          {:key (str rnum "-" cnum)}
                                          [:div
                                                util/abbreviate-text 
                                                [:span {:title element} 
                                                      element]]))
                                      row
                                      (range (count row)))))
                   values
                   (range 4)))
              [:tr  {:key "ellipsis"}
                    (doall (map (fn [number]
                                 (vector :td {:class "has-text-centered"
                                              :key   (str "ellipsis-" number)}
                                         "⋮ "))
                               (range (count attributes))))]]]]]))

(defn select-single-attribute [attribute]
  (let [mouse-pressed @(re-frame/subscribe [::subs/tmp])]
    [:tr (merge
            (if 
              (second attribute)
              {:class "is-selected"}
              {})
            (if 
              mouse-pressed
              {:style {:cursor "pointer"}}
              {})
            {:on-mouse-down #(re-frame/dispatch 
                              [::events/attribute-selection-down attribute])
             :on-mouse-up   #(re-frame/dispatch 
                              [::events/attribute-selection-up attribute])
             :key attribute})
      [:td (first attribute)]]))

(defn select-attributes [attributes]
  [:div {:class "box"
         :style {:height "450px"}}
    [:div {:class "level is-marginless"}
      [:h1 {:class "content title"}
        "Attributes to scale"]
      [util/tooltip "Try holding down the mouse button to multiselect!"]]
    [:div {:class "table-container is-unselectable box is-paddingless"
           :style {:height      "350px"
                   :overflow    "auto"}} 
      [:table {:class "table is-fullwidth is-hoverable is-bordered"}
        [:tbody
          (doall (map select-single-attribute attributes))]]]])

(defn select-panel []
  (let [attributes (re-frame/subscribe [::subs/ctx-attributes])]
    [:div {:class "container is-fluid"}
      [select-preview]
      [:div {:class "columns"}
        [:div {:class "column"}
          [select-attributes @attributes]]
        [:div {:class "column is-one-quarter"}
          [:div {:class "box has-text-centered"}
          [:button (merge 
                     {:type "button"
                      :class "button is-large is-info"
                      :on-click #(re-frame/dispatch
                                 [::events/initiate-attributes nil])}
                     ;; test if all attributes are unselected
                     (if (some identity (map second @attributes))
                       {}
                       {:disabled true}))
                   "Select"]]]]]))

;;;-Scale-Attribute-Selection--------------------------------------------------

(defn scale-export-button []
  [:div {:class "tile is-child box has-text-centered"}
      [:button {:class "button is-medium is-info"
                :on-click #(re-frame/dispatch
                            [::events/set-export-warning nil])}
                "Export"]])

(defn scale-previous-attribute-button [current-attribute]
  [:div {:class "column"}
    [:button {:class "button is-medium is-info is-fullwidth"
              :on-click #(re-frame/dispatch
                          [::events/previous-attribute nil])}
      [:span {:class "icon"}
        [:i {:class "fas fa-arrow-left"}]]]])

(defn scale-next-attribute-button [current-attribute]
  [:div {:class "column"}
    [:button {:class "button is-medium is-info is-fullwidth"
              :on-click #(re-frame/dispatch
                          [::events/next-attribute nil])}
      [:span {:class "icon"}
        [:i {:class "fas fa-arrow-right"}]]]])

(defn scale-drop-down-selection [current-attribute]
  (let [attribute-list (re-frame/subscribe [::subs/attribute-list])]
    [:div {:class "select"}
      [:select {:value current-attribute
                :on-change #(re-frame/dispatch
                             [::events/set-current-attribute 
                               (-> % .-target .-value)])}
        (map #(vector :option {:key % }  %) @attribute-list)]]))

(defn scale-attribute-selection [current-attribute]
  [:div {:class "tile is-child box"}
    [:div {:class "columns"}
      [scale-previous-attribute-button current-attribute]
      [scale-next-attribute-button current-attribute]]
    [:div {:class "columns"}
      [:div {:class "column"}
        [:h2 {:class "subtitle is-pulled-right"}
          "Jump to:"]]
      [:div {:class "column is-half"}
        [scale-drop-down-selection current-attribute]]]])

(defn scale-attribute-selection-box [current-attribute]
  [:div {:class "tile is-vertical"}  
    [:div {:class "tile is-parent"}
      [scale-export-button]]
    [:div {:class "tile is-parent"}
      [scale-attribute-selection current-attribute]]])

;;;-Scale-Statistics-----------------------------------------------------------

(defn statistics-box [attribute]
  (let [measure (re-frame/subscribe [::subs/attribute-measure attribute])]
    [:div {:class "box"}
      [:h5 {:class "title is-5"} "Statistics"]]))

;;;-Scale-Ordinal-Drag---------------------------------------------------------

(defn ordinal-drag-remove-button [order]
  [:div {:class "s-fullwidth container has-text-centered"} 
    [:br]
    [:button {:class "button"
              :key "remove"
              :on-click 
                #(re-frame/dispatch [::events/remove-order order])}
             "-"]])

(defn ordinal-drag-single-element [order element]
  [:span {:title element}
    [:div {:class "level box is-marginless"}
      [:div (merge 
              {:class "container has-text-centered is-unselectable"
               :style 
                {:white-space "nowrap"
                 :overflow "hidden"
                 :text-overflow "ellipsis"
                 :z-index 10}}
              (assoc util/drag-default
                :on-drag-start 
                  #(.setData (.-dataTransfer %) 
                             "text/plain" 
                             element)
                :on-drag
                  (fn[a] (.preventDefault a)
                         (re-frame/dispatch
                           [::events/order-remove-element 
                             [(:pos order)
                              (.getData (.-dataTransfer a) "Text")]]))))
        element]]])

(defn ordinal-drag-single-relation [order pos]
  [:div {:class "level notification is-marginless is-paddingless"} 
    [:div (merge 
            {:class "container has-text-centered is-unselectable"}
            (assoc util/drag-default
              :draggable false
              :on-drop
                (fn[a] (.preventDefault a)
                       (re-frame/dispatch
                         [::events/order-add-element 
                           [(.getData (.-dataTransfer a) "Text")
                            (:pos order) 
                            pos]]))))
          [:div {:class "is-size-4"
                 :style {:transform "rotate(270deg)"}}
                (case (:relation order)
                  "<=" (gstring/unescapeEntities "&le;")
                  ">=" (gstring/unescapeEntities "&ge;")
                  (:relation order))]]])

(defn ordinal-drag-selection [order]
  [:div {:class "level"}
    [:div {:class "select is-fullwidth"}
      [:select {:defaultValue (:relation order)
                :on-change 
                 #(re-frame/dispatch 
                   [::events/set-relation [(:pos order)
                                           (-> % .-target .-value)]])}
        (map #(vector :option 
                      {:key (str (:pos order) %)} 
                      %) 
             ["<" "<=" "=" ">=" ">"])]]])

(defn ordinal-drag-single-order [attribute order]
  [:div {:class "container"}
    [ordinal-drag-selection order]
    (let [elements (map 
                     #(vector :div {:key (str (:pos order) % "_ele")} 
                                   [ordinal-drag-single-element order %])
                     (:elements order))
          drop-fields (map 
                        #(vector :div {:key %}
                                      [ordinal-drag-single-relation order %])
                        (range (+ 1 (count (:elements order)))))]
      (cons (first drop-fields) 
            (interleave elements (drop 1 drop-fields))))
    [ordinal-drag-remove-button order]])

(defn ordinal-drag-orders [attribute]
  (let [orders @(re-frame/subscribe [::subs/orders attribute])]
    [:div {:class "columns" :style {:overflow "auto"
                                    :scrollbar-x-position "top"}}
      (doall
        (map 
          #(if (some? %) [:div {:key (:pos %) :class "column is-2"} 
                           [ordinal-drag-single-order attribute %]])
          orders))
       [:div {:class "column"} 
         [:button {:class "button"
                   :key "add-one"
                   :on-click 
                     #(re-frame/dispatch [::events/add-order nil])}
                  "+"]]]))

(defn ordinal-drag-single-attribute [value]
  [:td (merge
         {:key value
          :style {:height "45px"}}
         (assoc
           util/drag-default
           :on-drag-start 
             #(.setData (.-dataTransfer %) "text/plain" value)))
    [:div util/abbreviate-text
      [:span {:title value} value]]])

(defn ordinal-drag-values [attribute]
  (let [values @(re-frame/subscribe [::subs/current-distinct attribute])]
    [:div {:class "table-container is-unselectable box is-paddingless"
           :style {:height    "150px"
                   :overflow  "auto"}} 
      [:table {:class "table is-fullwidth is-bordered"}
        [:tbody
          (doall 
            (map
              #(vector :tr {:key %} %)
              (partition-all 5
                (map ordinal-drag-single-attribute values))))]]]))

(defn ordinal-drag [attribute]
  [:div
    [ordinal-drag-values attribute]
    [ordinal-drag-orders attribute]
    [:button {:class "button"
              :on-click 
                #(re-frame/dispatch [::events/switch-to-context nil])}
             "Context view"]])

;;;-Scale-Ordinal-Context------------------------------------------------------

(defn ordinal-attribute-form [current-attribute]
  [:div {:class "field is-grouped"}
    [:div {:class "control"}
      [:input {:class "input" 
               :on-key-down #(if (= 13 (.-which %))
               (re-frame/dispatch
                           [::events/add-new-attribute 
                             (.-value (. js/document 
                                         getElementById 
                                         "new-attribute"))]))
               :id    "new-attribute"
               :type  "text" 
               :placeholder "New Attribute Name"}]]
    [:div {:class "control"}
      [:button {:class "button"
                :type  "button"
                :on-click #(re-frame/dispatch
                            [::events/add-new-attribute 
                              (.-value (. js/document 
                                          getElementById 
                                          "new-attribute"))])}
               "Add"]]])

(defn ordinal-table-buttons [attributes]
  [:table {:class "table is-scrollable is-unselectable is-marginless"}
    [:tbody
      [:tr 
        [:td {:key "empty"}
          [:div {:style {:width "75px" :height "40px"}}]]
      (map #(vector :td {:key %}
                    [:div {:style {:width "75px"}}
                      [:button {:class "button"
                                    :on-click 
                                     (fn [a] (re-frame/dispatch
                                               [::events/remove-attribute %]))} 
                                    "-"]]) 
           attributes)]]])

(defn ordinal-table [current-attribute]
  (let [attributes 
         @(re-frame/subscribe [::subs/current-attributes current-attribute])
        values     
         @(re-frame/subscribe [::subs/current-distinct current-attribute])
        incidence
         @(re-frame/subscribe [::subs/incidence current-attribute])]
    [:div {:class "table-container"}
      [ordinal-table-buttons attributes]
      [:table {:class "table is-bordered is-scrollable is-unselectable"}
        [:thead
          ;; first header element with input field
          [:tr [:th {:class "is-paddingless"}
                 [:input {:class "input is-marginless"
                          :style {:width "99px"
                                  :border 0
                                  :box-shadow "none"}
                          :on-change #(re-frame/dispatch
                                       [::events/relation-name
                                         (-> % .-target .-value)])
                          :placeholder "Relation"
                          :value @(re-frame/subscribe
                                   [::subs/relation 
                                     current-attribute])}]]
               ;; remaining headers with drag and drop
               (map (fn [value]
                      (vector 
                        :th {:key value} 
                        [:div (merge
                                util/abbreviate-text
                                (assoc
                                  util/drag-default
                                  :on-drag-start 
                                    #(.setData (.-dataTransfer %) 
                                               "text/plain" 
                                               value)
                                  :on-drop
                                    (fn[a] (.preventDefault a)
                                           (re-frame/dispatch
                                             [::events/drop-column 
                                               [(.getData 
                                                   (.-dataTransfer a) "Text")
                                                value]]))))
                        [:span {:title value} value]])) 
                    attributes)]]
        [:tbody
          ;; all distinct attributes with drag and drops
          (map
            (fn [value]
              [:tr {:key value}
                [:td {:key value} 
                  [:div (merge
                          util/abbreviate-text
                          (assoc
                            util/drag-default
                            :on-drag-start 
                              #(.setData (.-dataTransfer %) "text/plain" value)
                            :on-drop
                              (fn[a] (.preventDefault a)
                                     (re-frame/dispatch
                                       [::events/drop-row 
                                         [(.getData (.-dataTransfer a) "text")
                                          value]]))))
                [:span {:title value} [:b value]]]]
                ;; the incidence of the current distinct object
                (map
                  (fn [attribute] 
                   (vector :td {:class "has-text-centered"
                                :key attribute
                                :on-click #(re-frame/dispatch 
                                            [::events/swap-incidence 
                                              [attribute value]])}
                               (if (get-in incidence [(keyword attribute) 
                                                      (keyword value)]) 
                                   "X" )))
                  attributes)])
            values)]]]))

;;;-Scale-Ordinal--------------------------------------------------------------

(defn ordinal-scale [current-attribute]
  (let [is-context @(re-frame/subscribe 
                     [::subs/context-view current-attribute])]
    [:div
      [:div {:class "box"}
        [:h5 {:class "title is-5"} "Ordinal Scaling"]
        (if is-context
          [:div [ordinal-attribute-form current-attribute]
                [ordinal-table current-attribute]
                [:button {:class "button"
                          :on-click 
                            #(re-frame/dispatch 
                              [::events/switch-to-drag nil])}
                         "Reset"]]
          [ordinal-drag current-attribute])]]))

;;;-Scale-Numeric--------------------------------------------------------------

(defn numeric-generate-attributes [current-attribute]
  [:div {:class "tile is-anchestor is-vertical"}
    [:div {:class "tile is-parent has-text-weight-bold 
                   is-marginless is-paddingless"}
      [:div {:class "is-marginless is-paddingless"
             :style {:width "200px"}}
           "Number of intervals:"]
      [:div {:class "is-marginless is-paddingless"} 
           "Type:"]]
    [:div {:class "tile is-parent field has-addons 
                   is-marginless is-paddingless"}
      [:div {:class "control"}
        [:input {:class "input"
                 :id    "interval-input"    
                 :style {:width "200px"}}]]
      [:div {:class "control"}
        [:div {:class "select"}
          [:select {:id "interval-select"}
            [:option "equal length"]
            [:option "equally distributed"]]]]
      [:div {:class "control"}
         [:button {:class "button"
                   :on-click 
                     #(re-frame/dispatch
                       [::events/generate-intervals
                         (vector
                          (.-value (. js/document 
                                         getElementById 
                                         "interval-input")) 
                          (.-value (. js/document 
                                         getElementById 
                                         "interval-select")))])} 
                  "Generate"]]]
      [:div {:class "tile is-parent"}]])


(defn numeric-attribute-select-button [attribute]
  [:button {:class "button is-pulled-right"
            :on-click #(re-frame/dispatch
                        [::events/select-attribute attribute])
            :style {:width "160px"}} 
           "select Attribute"])

(defn numeric-attribute-select-all-button [attributes]
  [:button {:class "button is-fullwidth"
            :on-click 
            #(doall
              (map
                (fn [attribute] (re-frame/dispatch
                                  [::events/select-attribute attribute]))
                (filter (fn [a](some? a)) attributes)))} 
           "select all Attributes"])

(defn numeric-single-interval-left [attribute interval listkey]
  [:div {:class "column is-narrow is-marginless is-paddingless"}
    [:div {:class "field has-addons"}
      [:div {:class "control"}
        [:button 
          (merge
            (if (= "(" (:left interval))
              {:class "button is-marginless"}
              {:class "button is-marginless has-background-dark
                       has-text-white"})
            {:on-click 
              #(re-frame/dispatch 
                [::events/swap-bracket-left [attribute interval listkey]])})
          [:b (:left interval)]]]
      [:div {:class "control"}
        [:input {:class "input is-marginless"
                 :value (:start interval)
                 :on-change 
                   #(re-frame/dispatch 
                     [::events/set-number-start 
                       [attribute 
                        interval 
                        (-> % .-target .-value)
                        listkey]])}]]]])

(defn numeric-single-interval-right [attribute interval listkey]
  [:div {:class "column is-narrow is-marginless is-paddingless"}
    [:div {:class "field has-addons"}
      [:div {:class "control"}
        [:input {:class "input is-marginless"
                 :value (:end interval)
                 :on-change
                   #(re-frame/dispatch 
                     [::events/set-number-end 
                       [attribute 
                        interval 
                        (-> % .-target .-value) 
                        listkey]])}]]
      [:div {:class "control"}
        [:button 
          (merge
            (if (= ")" (:right interval))
              {:class "button is-marginless"}
              {:class "button is-marginless has-background-dark 
                       has-text-white"})
            {:on-click 
              #(re-frame/dispatch 
                [::events/swap-bracket-right [attribute interval listkey]])})
            [:b (:right interval)]]]]])

(defn numeric-single-interval [attribute interval listkey]
  [:div (merge 
          {:class "tile is-anchestor"}
          (assoc util/drag-default
               :on-drag-start 
                 #(.setData (.-dataTransfer %) 
                            "text/plain" 
                            (str (:left interval) "," (:start interval) ","
                                 (:end interval) "," (:right interval)))
               :on-drag (fn[a] (.preventDefault a)
                          (re-frame/dispatch 
                            [::events/remove-interval 
                              [attribute interval listkey]]))))
    [:div {:class "tile is-child is-8 columns"}
      [numeric-single-interval-left attribute interval listkey]  
      [:div {:class "column is-narrow"}
        "-"]
      [numeric-single-interval-right attribute interval listkey]]
    [:div {:class "tile is-child is-2"}
      [:button {:class "button is-pulled-right"
                :on-click #(re-frame/dispatch 
                            [::events/remove-interval 
                              [attribute interval listkey]])} 
               "-"]]
    [:div {:class "tile is-child is-2"}        
      [:span {:class "icon is-pulled-right"}
        [:i {:class "fas fa-arrows-alt"}]]]])

(defn numeric-single-attribute [attribute listkey]
  [:div (merge 
          (assoc 
            util/drag-default
            ;needed to make the nested divs draggable
            :on-drag nil
            :on-drag-start nil
            :draggable false
            :on-drop 
              (fn[a] (.preventDefault a)
                     (re-frame/dispatch 
                       [::events/insert-interval 
                         [attribute 
                          (.getData (.-dataTransfer a) "Text")
                          listkey]])))
          {:class "box"})
    [:div {:class "columns"}
      [:div {:class "column"} 
        [:input {:class "input is-marginless has-text-weight-bold"
                 :style {:width "150px"
                         :border 0
                         :box-shadow "none"}
                 :on-change #(re-frame/dispatch
                              [::events/set-interval-attribute-name
                                [attribute
                                 (-> % .-target .-value)
                                 listkey]])
                 :placeholder "Null"
                 :value (:name attribute)}]
        (if @(re-frame/subscribe 
                  [::subs/multiple-names [(:name attribute) listkey]])
            [:span {:class "icon is-medium has-text-warning"
                    :title "Name is not unique!"} 
              [:i {:class "fas fa-lg fa-exclamation-triangle"}]])]
      [:div {:class "column"} [:button {:class "button is-pulled-right" 
                                        :on-click 
                                          #(re-frame/dispatch
                                            [::events/remove-interval-attribute 
                                              [attribute listkey]])
                                        :style {:width "160px"}} 
                                "remove Attribute"]]]
    (doall
      (map
        (fn [interval] 
          (if (some? interval)
            (vector 
              :div {:key (str (:pos attribute) "-" 
                              (:pos interval))}
              [numeric-single-interval attribute interval listkey])))
        (:intervals attribute)))
    [:button {:class "button"
              :on-click #(re-frame/dispatch 
                          [::events/add-interval [attribute listkey]])} 
             "+"]
    (if (= listkey :generated)
        [numeric-attribute-select-button attribute])])

(defn numeric-interval-attribute-button [current-attribute listkey] 
  [:button {:class "button is-fullwidth"
            :on-click #(re-frame/dispatch
                        [::events/add-interval-attribute listkey])}
    "new Attribute"])

(defn numeric-intervals [current-attribute listkey]
  (let [attributes @(re-frame/subscribe 
                     [::subs/selected-attributes [current-attribute listkey]])]
    [:div {:class "tile is-anchestor is-vertical"}
      (doall
        (map
          (fn [attribute] 
            (if (some? attribute)
                (vector :div {:class "tile is-child"
                              :key (str "selected-" (:pos attribute))} 
                        [numeric-single-attribute attribute listkey])))
          attributes))
      [:div {:class "columns"}
        [:div {:class "column"} 
          [numeric-interval-attribute-button current-attribute listkey]]
        (if (= listkey :generated)
          [:div {:class "column"}
            [numeric-attribute-select-all-button attributes]])]]))

;;;-Scale-Numeric--------------------------------------------------------------

(defn numeric-scale [current-attribute]
  [:div
    [:div {:class "box"}
      [:h5 {:class "title is-5"} "Selected Intervals"]
      [numeric-intervals current-attribute :selected]]
    [:div {:class "box"}
      [:h5 {:class "title is-5"} "Generate Intervals"]
      [numeric-generate-attributes current-attribute]
      [numeric-intervals current-attribute :generated]]])

;;;-Scale-Scaling--------------------------------------------------------------

(defn scaling-box [current-attribute]
  (let [measure @(re-frame/subscribe 
                  [::subs/attribute-measure current-attribute])]
    (if (not (= measure "nominal"))
        (case measure
          "ordinal"   [ordinal-scale current-attribute]
          "numeric"   [numeric-scale current-attribute]
          nil))))

;;;-Scale-Header---------------------------------------------------------------

(defn scale-measurement-radio [current-attribute]
  (let [measure @(re-frame/subscribe 
                  [::subs/attribute-measure current-attribute])
        numerical-bool @(re-frame/subscribe 
                         [::subs/attribute-numerical current-attribute])]
    [:div {:class "container is-fullwidth is-unselectable"}
      [:table {:class "table is-fullwidth is-hoverable"}
        [:thead
          [:tr
            [:th "Level of Measurement"]]]
        [:tbody
          (doall 
            (map 
              #(vector
                :tr (if (and (= "numeric" %)
                             (not numerical-bool))
                      {:key %
                       :class "has-background-grey-lighter"}
                      (merge 
                        (if (= measure %) {:class "is-selected"} {})
                        {:on-click (fn [_] (re-frame/dispatch
                                           [::events/change-measure %]))
                        :key %}))
                  [:td (capitalize %)])
              ["nominal" "ordinal" "numeric"]))]]]))

(defn scale-measurement-selection-box [current-attribute]
  [:div {:class "tile is-parent"}
    [:div {:class "tile is-child box"}
      [:h1 {:class "title"}
        current-attribute]
      [scale-measurement-radio current-attribute]]])

(defn scale-header [current-attribute]
  [:div {:class "tile is-ancestor"}
    [:div {:class "tile is-7"}
      [scale-measurement-selection-box current-attribute]]
    [:div {:class "tile"}
      [scale-attribute-selection-box current-attribute]]])

(defn scale-panel []
  (let [current-attribute @(re-frame/subscribe [::subs/current-attribute])]
    [:div {:class "container is-fluid"} 
      [scale-header current-attribute]
      [statistics-box current-attribute]
      [scaling-box current-attribute]]))

;;;-Export---------------------------------------------------------------------

(defn export-warning []
  [:div {:class "tile is-parent is-fullwidth"}
    [:div {:class "container box"}
      [:h1 {:class "title"} "Caution!"]
      [:p "The Download-Buttons still link to previously generated files,"
          " generate them again to include possible changes."]]])

(defn export-context [url]
  (let [title (re-frame/subscribe [::subs/ctx-file-name])]
    [:div {:class "tile is-parent"}
      [:div {:class "container box has-text-centered"}
        [:h1 {:class "title"}
          "Scaled Context"]
        [:button {:class "button is-large is-info is-fullwidth"
                  :on-click #(re-frame/dispatch [::events/make-context nil])}
          "Generate"]
        [:br]
        [:a {:href url
             :download (str (first (split @title #"\.")) ".ctx")}             
          [:button (merge
                     {:type "button"}
                     (if url
                       {:class "button is-large is-info is-fullwidth"}
                       {:class "button is-large is-info is-fullwidth"
                        :disabled true}))
              [:i {:class "fas fa-download"}] "Download"]]]]))

(defn export-config [url]
  [:div {:class "tile is-parent"}
    [:div {:class "container box has-text-centered"}
      [:h1 {:class "title"}
        "Import File"]
      [:button {:class "button is-large is-info is-fullwidth"
                :on-click #(re-frame/dispatch [::events/make-config nil])}
        "Generate"]
      [:br]
      [:a {:href url
           :download "config.json"}             
        [:button (merge
                   {:type "button"}
                   (if url
                     {:class "button is-large is-info is-fullwidth"}
                     {:class "button is-large is-info is-fullwidth"
                      :disabled true}))
            [:i {:class "fas fa-download"}] "Download"]]]])

(defn export-panel []
  (let [warning       (re-frame/subscribe [::subs/warning])
        context-url   @(re-frame/subscribe [::subs/context-url])
        config-url    @(re-frame/subscribe [::subs/config-url])]
    [:div {:class "container is-fluid"}
      [:div {:class "tile is-ancestor is-vertical"}
        (if (and @warning (or context-url config-url)) [export-warning])
        [:div {:class "tile"}
          [export-context context-url] 
          [export-config config-url]]]]))

;;;-Main-----------------------------------------------------------------------

(defn main-panel []
  (let [panel @(re-frame/subscribe [::subs/panel])]
    [:div {:class "container"}
      [header panel]
      (case panel
        "import"  [import-panel]
        "select"  [select-panel]
        "scale"   [scale-panel]
        "export"  [export-panel]
        (print panel))
      ;; the empty hero is used to allow scrolling past scaling elements
      [:div {:class "hero is-fullheight"}]]))

