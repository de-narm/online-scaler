(ns online-scaler.views
  (:require
   [clojure.string :refer [capitalize split]]
   [re-frame.core :as re-frame]
   [online-scaler.events :as events]
   [online-scaler.subs :as subs]))

;;;-Header---------------------------------------------------------------------

(defn header [current-panel]
  (let [can-select @(re-frame/subscribe [::subs/mv-ctx-attributes])
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
        @(re-frame/subscribe [::subs/mv-ctx-file-name])]]])

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
        @(re-frame/subscribe [::subs/import-name])]]])

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

(defn select-single-attribute [attribute]
  [:tr (merge
          (if 
            (second attribute)
            {:class "is-selected"}
            {})
          {:on-mouse-down #(re-frame/dispatch 
                            [::events/attribute-selection-down attribute])
           :on-mouse-up   #(re-frame/dispatch 
                            [::events/attribute-selection-up attribute])
           :key attribute})
    [:td (first attribute)]])

(defn select-attributes []
  (let [attributes (re-frame/subscribe [::subs/mv-ctx-attributes])]
    [:div {:class "box"
           :style {:height "450px"}}
      [:h1 {:class "content title"}
        "Attributes to scale"]
      [:div {:class "table-container is-unselectable box is-paddingless"
             :style {:height      "350px"
                     :overflow    "auto"}} 
        [:table {:class "table is-fullwidth is-hoverable is-bordered"}
          [:tbody
            (map select-single-attribute @attributes)]]]]))

(defn select-panel []
  [:div {:class "container is-fluid"}
    [:div {:class "columns"}
      [:div {:class "column"}
        [select-attributes]]
      [:div {:class "column is-one-quarter"}
        [:div {:class "box has-text-centered"}
        [:button {:type "button"
                  :class "button is-large is-info"
                  :on-click #(re-frame/dispatch
                             [::events/initiate-attributes nil])}
                 "Select"]]]]])

;;;-Scale-Attribute-Selection--------------------------------------------------

(defn scale-export-button []
  [:div {:class "tile is-child box has-text-centered"}
      [:button {:class "button is-medium is-info"
                :on-click #(re-frame/dispatch
                            [::events/set-panel "export"])}
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
                  [:td %])
              ["Nominal" "Ordinal" "Numeric"]))]]]))

(defn scale-measurement-selection-box [current-attribute]
  [:div {:class "tile is-parent"}
    [:div {:class "tile is-child box"}
      [:h1 {:class "title"}
        current-attribute]
      [scale-measurement-radio [current-attribute]]]])

(defn scale-header [current-attribute]
  [:div {:class "tile is-ancestor"}
    [:div {:class "tile is-7"}
      [scale-measurement-selection-box current-attribute]]
    [:div {:class "tile"}
      [scale-attribute-selection-box current-attribute]]])

(defn scale-panel []
  (let [current-attribute (re-frame/subscribe [::subs/current-attribute])]
    [:div {:class "container is-fluid"} 
      [scale-header @current-attribute]
      [:div {:class "box"} "statistics etc" [:br]]]))

;;;-Export---------------------------------------------------------------------

(defn export-warning []
  [:div {:class "tile is-parent is-fullwidth"}
    [:div {:class "container box"}
      [:h1 {:class "title"} "Caution!"]
      [:p "The Download-Buttons still link to previously generated files,"
          " generate them again to include possible changes."]]])

(defn export-context [url]
  (let [title @(re-frame/subscribe [::subs/mv-ctx-file-name])]
    [:div {:class "tile is-parent"}
      [:div {:class "container box has-text-centered"}
        [:h1 {:class "title"}
          "Scaled Context"]
        [:button {:class "button is-large is-info is-fullwidth"
                  :on-click #(re-frame/dispatch [::events/make-context nil])}
          "Generate"]
        [:br]
        [:a {:href url
             :download (str (first (split title #"\.")) ".ctx")}             
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
  (let [warning       @(re-frame/subscribe [::subs/warning])
        context-url   @(re-frame/subscribe [::subs/context-url])
        config-url    @(re-frame/subscribe [::subs/config-url])]
    [:div {:class "container is-fluid"}
      [:div {:class "tile is-ancestor is-vertical"}
        (if (and warning (or context-url config-url)) [export-warning])
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
