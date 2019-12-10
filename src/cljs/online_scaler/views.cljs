(ns online-scaler.views
  (:require
   [clojure.string :refer [capitalize split]]
   [re-frame.core :as re-frame]
   [online-scaler.events :as events]
   [online-scaler.subs :as subs]))

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
    [:div {:class "table-container is-unselectable box"}
      [:h1 {:class "title"} "Preview"]
      [:table {:class "table is-bordered"}
        [:thead 
          [:tr (doall (map #(vector :th {:key %} 
                              [:div {:style 
                                      {:width "75px"
                                       :white-space "nowrap"
                                       :overflow "hidden"
                                       :text-overflow "ellipsis"}}
                                     %]) 
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
                                          {:style 
                                            {:width "75px"
                                             :white-space "nowrap"
                                             :overflow "hidden"
                                             :text-overflow "ellipsis"}}
                                              element]))
                                    row
                                    (range (count row)))))
                 values
                 (range 4)))
            [:tr  {:key "ellipsis"}
                  (doall (map (fn [number]
                               (vector :td {:class "has-text-centered"
                                            :key   (str "ellipsis-" number)}
                                       "⋮ "))
                             (range (count attributes))))]]]]))

(defn select-single-attribute [attribute]
  (let [mouse-pressed @(re-frame/subscribe [::subs/get-tmp])]
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
    [:h1 {:class "content title"}
      "Attributes to scale"]
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

(defn ordinal-drag-single-attribute [value]
  [:td {:key value
        :draggable "true"
        :on-drag #(.preventDefault %)
        :on-drag-over #(.preventDefault %)
        :on-drag-enter #(.preventDefault %)
        :on-drag-leave #(.preventDefault %)
        :on-drag-end #(.preventDefault %)
        :on-drop #(.preventDefault %)
        :on-drag-start 
          #(.setData (.-dataTransfer %) 
                     "text/plain" 
                     value)}
    [:div {:style 
           {:width "140px"
            :white-space "nowrap"
            :overflow "hidden"
            :text-overflow "ellipsis"}}
    value]])

(defn ordinal-drag-values [attribute]
  (let [values @(re-frame/subscribe [::subs/current-distinct attribute])]
    [:div {:class "table-container is-unselectable box is-paddingless"
           :style {:height    "150px"
                   :overflow  "auto"}} 
      [:table {:class "table is-fullwidth is-bordered"}
        [:tbody
          (doall 
            (map
              #(vector :tr %)
              (partition-all 5
                (map ordinal-drag-single-attribute values))))]]]))

(defn ordinal-drag [attribute]
  [ordinal-drag-values attribute])

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
      [:button {:class "button is-info"
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
          [:div {:style {:width "75px"}}]]
      (map #(vector :td {:key %}
                    [:div {:style 
                            {:width "75px"
                             :white-space "nowrap"
                             :overflow "hidden"
                             :text-overflow "ellipsis"}}
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
         @(re-frame/subscribe [::subs/get-incidence current-attribute])]
    [:div {:class "table-container"}
      [ordinal-table-buttons attributes]
      [:table {:class "table is-bordered is-scrollable is-unselectable"}
        [:thead
          ;; first header element with input field
          [:tr [:th [:input {:class "input is-small"
                             :style {:width "75px"}
                             :on-change #(re-frame/dispatch
                                          [::events/relation-name
                                            (-> % .-target .-value)])
                             :value @(re-frame/subscribe
                                      [::subs/get-relation 
                                        current-attribute])}]]
               ;; remaining headers with drag and drop
               (map (fn [value]
                      (vector 
                        :th {:key value} 
                        [:div {:draggable "true"
                               :on-drag #(.preventDefault %)
                               :on-drag-over #(.preventDefault %)
                               :on-drag-enter #(.preventDefault %)
                               :on-drag-leave #(.preventDefault %)
                               :on-drag-end #(.preventDefault %)
                               :on-drag-start 
                                 #(.setData (.-dataTransfer %) 
                                            "text/plain" 
                                            value)
                               :on-drop
                                 (fn[a] (.preventDefault a)
                                        (re-frame/dispatch
                                          [::events/drop-column 
                                            (vector
                                              (.getData 
                                                (.-dataTransfer a) "Text")
                                              value)]))
                              :style 
                               {:width "75px"
                                :white-space "nowrap"
                                :overflow "hidden"
                                :text-overflow "ellipsis"}}
                        value])) 
                    attributes)]]
        [:tbody
          ;; all distinct attributes with drag and drops
          (map
            (fn [value]
              [:tr {:key value}
                [:td {:key value} 
                  [:div {:draggable "true"
                         :on-drag #(.preventDefault %)
                         :on-drag-over #(.preventDefault %)
                         :on-drag-enter #(.preventDefault %)
                         :on-drag-leave #(.preventDefault %)
                         :on-drag-end #(.preventDefault %)
                         :on-drag-start 
                           #(.setData (.-dataTransfer %) "text/plain" value)
                         :on-drop
                           (fn[a] (.preventDefault a)
                                  (re-frame/dispatch
                                    [::events/drop-row 
                                      (vector
                                        (.getData (.-dataTransfer a) "text")
                                        value)]))
                         :style 
                           {:width "75px"
                            :font-weight "bold"
                            :white-space "nowrap"
                            :overflow "hidden"
                            :text-overflow "ellipsis"}}
                value]]
                ;; the incidence of the current distinct object
                (map
                  (fn [attribute] 
                   (vector :td {:class "has-text-centered"
                                :key attribute
                                :on-click #(re-frame/dispatch 
                                            [::events/swap-incidence 
                                              (vector attribute value)])}
                               (if (get-in incidence [(keyword attribute) 
                                                      (keyword value)]) 
                                   "X" )))
                  attributes)])
            values)]]]))

;;;-Scale-Ordinal--------------------------------------------------------------

(defn ordinal-scale [current-attribute]
  [:div
    (if @(re-frame/subscribe [::subs/context-view current-attribute])
        [:div [ordinal-attribute-form current-attribute]
              [ordinal-table current-attribute]]
        [ordinal-drag current-attribute])])

;;;-Scale-Scaling--------------------------------------------------------------

(defn scaling-box [current-attribute]
  (let [measure @(re-frame/subscribe 
                  [::subs/attribute-measure current-attribute])]
    (if (not (= measure "nominal"))
      [:div {:class "box"}
        [:h5 {:class "title is-5"} "Scaling"]
        (case measure
          "ordinal"   [ordinal-scale current-attribute]
          nil)])))

;;;-Scale-Header---------------------------------------------------------------

(defn scale-measurement-radio [current-attribute]
  (let [measure (re-frame/subscribe 
                  [::subs/attribute-measure current-attribute])]
    [:div {:class "container is-fullwidth is-unselectable"}
      [:table {:class "table is-fullwidth is-hoverable"}
        [:thead
          [:tr
            [:th "Level of Measurement"]]]
        [:tbody
          (doall 
            (map 
              #(vector
                :tr (merge 
                      (if (= @measure %) {:class "is-selected"} {})
                      {:on-click (fn [_] (re-frame/dispatch
                                           [::events/change-measure %]))
                       :key %})
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
  (let [panel (re-frame/subscribe [::subs/panel])]
    [:div {:class "container"}
      [header @panel]
      (case @panel
        "import"  [import-panel]
        "select"  [select-panel]
        "scale"   [scale-panel]
        "export"  [export-panel]
        (print @panel))]))
