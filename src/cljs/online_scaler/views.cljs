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
				[:a "Import"]]]])

(defn upload-file []
	[:div {:class "file has-name is-boxed is-large is-centered"}
		[:label {:class "file-label"}
			[:input {:class "file-input" :type "file"
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
  [:div 
    [upload-file]
    [upload-checkbox]
    [:br]
    [:button {:type "button"
              :on-click #(re-frame/dispatch
                          [::events/make-mv-ctx nil])}
      "scale"]])

(defn upload-panel []
  [:div {:class "container is-centered has-text-centered"}
		[upload-header]
    [upload-form]])

;;; Main

(defn main-panel []
  (let [mv-ctx (re-frame/subscribe [::subs/mv-ctx])]
    (if (nil? @mv-ctx)
      [upload-panel]
      (print @mv-ctx))))
