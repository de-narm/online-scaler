(ns online-scaler.views
  (:require
   [re-frame.core :as re-frame]
   [online-scaler.events :as events]
   [online-scaler.subs :as subs]))

;;;-Upload---------------------------------------------------------------------

(defn upload-header []
	[:div {:class "tabs is-centered"}
		[:ul
			[:li {:class "is-active"} 
				[:a "Upload"]]
			[:li  
				[:a {:on-click #(re-frame/dispatch
                         [::events/set-panel "import"])}
         "Import"]]]])

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
	[:label {:class "checkbox"}
		[:input {:type "checkbox"
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
                  :class "button is-large is-info"
                  :on-click #(re-frame/dispatch
                             [::events/initiate-selection nil])}
                 "Select"])]]))

(defn upload-panel []
  [:div {:class "container is-fluid"}
		[upload-header]
    [upload-form]])

;;;-Import---------------------------------------------------------------------

(defn import-header []
	[:div {:class "tabs is-centered"}
		[:ul
			[:li 
				[:a {:on-click #(re-frame/dispatch
                         [::events/set-panel "upload"])}
         "Upload"]]
			[:li {:class "is-active"}  
				[:a "Import"]]]])

(defn import-panel []
  [:div {:class "container is-fluid"}
		[import-header]])

;;;-Select---------------------------------------------------------------------

(defn select-single-attribute [attribute]
  [:tr (merge
          (if 
            (second attribute)
            {:class "is-selected"}
            {})
          {:on-click #(re-frame/dispatch 
                       [::events/attribute-selection-swap attribute])
           :key attribute})
    [:td (first attribute)]])

(defn select-attributes []
  (let [attributes (re-frame/subscribe [::subs/mv-ctx-attributes])]
    [:div {:class "box"
           :style {:height "450px"}}
      [:h1 {:class "content title"}
        "Attributes to scale"]
      [:div {:class "table-container is-unselectable"
             :style {:height      "350px"
                     :overflow    "auto"}} 
        [:table {:class "table is-fullwidth is-hoverable"}
          [:tbody
            (map select-single-attribute @attributes)]]]]))

(defn select-panel []
  [:div {:class "container is-fluid"}
    [:br]
    [:div {:class "columns"}
      [:div {:class "column"}
        [select-attributes]]
      [:div {:class "column is-one-quarter"}
        [:div {:class "box has-text-centered"}
        [:button {:type "button"
                  :class "button is-large is-info"
                  :on-click #(re-frame/dispatch
                             [::events/initiate-attributes nil])}
                 "Scale"]]]]])

;;;-Scaling--------------------------------------------------------------------

(defn scaling-export-button []
  [:div {:class "tile is-child box has-text-centered"}
      [:button {:class "button is-medium is-info"
                :on-click #(re-frame/dispatch
                            [::events/set-panel "export"])}
                "Export"]])

(defn scaling-previous-attribute-button [current-attribute]
  [:div {:class "column"}
    [:button {:class "button is-medium is-info is-fullwidth"
              :on-click #(re-frame/dispatch
                          [::events/previous-attribute nil])}
      [:span {:class "icon"}
        [:i {:class "fas fa-arrow-left"}]]]])

(defn scaling-next-attribute-button [current-attribute]
  [:div {:class "column"}
    [:button {:class "button is-medium is-info is-fullwidth"
              :on-click #(re-frame/dispatch
                          [::events/next-attribute nil])}
      [:span {:class "icon"}
        [:i {:class "fas fa-arrow-right"}]]]])

(defn scaling-drop-down-selection [current-attribute]
  (let [attribute-list (re-frame/subscribe [::subs/attribute-list])]
  [:div {:class "select"
         :on-change #(re-frame/dispatch
                      [::events/set-current-attribute 
                        (-> % .-target .-value)])}
    [:select
      (map #(vector :option {:key %}  %) @attribute-list)]]))

(defn scaling-attribute-selection [current-attribute]
  [:div {:class "tile is-child box"}
    [:div {:class "columns"}
      [scaling-previous-attribute-button current-attribute]
      [scaling-next-attribute-button current-attribute]]
    [:div {:class "columns"}
      [:div {:class "column"}
        [:h2 {:class "subtitle is-pulled-right"}
          "Jump to:"]]
      [:div {:class "column is-half"}
        [scaling-drop-down-selection current-attribute]]]])

(defn scaling-attribute-selection-box [current-attribute]
  [:div {:class "tile is-vertical"}  
    [:div {:class "tile is-parent"}
      [scaling-export-button]]
    [:div {:class "tile is-parent"}
      [scaling-attribute-selection current-attribute]]])

(defn scaling-measurement-radio [current-attribute]
  (let [measure (re-frame/subscribe 
                  [::subs/attribute-measure current-attribute])]
    [:div {:class "container is-fullwidth"}
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

(defn scaling-measurement-selection-box [current-attribute]
  [:div {:class "tile is-parent"}
    [:div {:class "tile is-child box"}
      [:h1 {:class "title"}
        current-attribute]
      [scaling-measurement-radio [current-attribute]]]])

(defn scaling-header [current-attribute]
  [:div {:class "tile is-ancestor"}
    [:div {:class "tile is-8"}
      [scaling-measurement-selection-box current-attribute]]
    [:div {:class "tile"}
      [scaling-attribute-selection-box current-attribute]]])

(defn scaling-panel []
  (let [current-attribute (re-frame/subscribe [::subs/current-attribute])]
    [:div {:class "container is-fluid"} 
      [:br]
      [scaling-header @current-attribute]
      [:div {:class "box"} "stastics etc" (repeat 20 [:br])]]))

;;;-Export---------------------------------------------------------------------

(defn export-panel []
  [:div {:class "box is-fullwidth"}
    [:button {:type "button"
              :class "button is-large is-info"
              :on-click #(re-frame/dispatch
                          [::events/export-make-context nil])}
            "Generate"]
    [:br]
    [:br]
    (let [url (re-frame/subscribe [::subs/url])]
      [:a {:href @url
           :download "scaled_ctx.ctx"}             
          "Export"])])

;;;-Main-----------------------------------------------------------------------

(defn main-panel []
  (let [panel (re-frame/subscribe [::subs/panel])]
    (case @panel
      "upload"  [upload-panel]
      "import"  [import-panel]
      "select"  [select-panel]
      "scaling" [scaling-panel]
      "export"  [export-panel]
      (print @panel))))
