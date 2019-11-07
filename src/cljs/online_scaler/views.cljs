(ns online-scaler.views
  (:require
   [re-frame.core :as re-frame]
   [online-scaler.events :as events]
   [online-scaler.subs :as subs]))

;;; Upload

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

;;; Import

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

;;; Select

(defn select-single-attribute [attribute]
  [:div (merge
          (if 
            (second attribute)
            {:class "column box is-full has-background-primary 
                     has-text-white"}
            {:class "column box is-full has-background-grey-dark
                     has-text-white"})
          {:on-click #(re-frame/dispatch 
                       [::events/attribute-selection-swap attribute])
           :key (first attribute)})
    (first attribute)])

(defn select-attributes []
  (let [attributes (re-frame/subscribe [::subs/mv-ctx-attributes])]
    [:div {:class "box"
           :style {:height "350px"
                   :overflow "auto"
                   :user-select "none"}} 
      [:div {:class "columns is-8 is-multiline"}
        (map select-single-attribute @attributes)]]))

(defn select-panel []
  [:div {:class "container is-fluid"}
    [:br]
    [:div {:class "columns"}
      [:div {:class "column"}
        [select-attributes]]
      [:div {:class "column is-one-quarter"}
        [:button {:type "button"
                  :class "button is-large is-info"
                  :on-click #(re-frame/dispatch
                             [::events/set-panel "export"])}
                 "Scale"]]]])

;;; Export

(defn export-panel []
  [:div 
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

;;; Main

(defn main-panel []
  (let [panel (re-frame/subscribe [::subs/panel])]
    (case @panel
      "upload"  [upload-panel]
      "import"  [import-panel]
      "select"  [select-panel]
      "export"  [export-panel]
      (print @panel))))
