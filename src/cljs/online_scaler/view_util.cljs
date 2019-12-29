(ns online-scaler.view-util)

;;;

;; used for all tables
(def abbreviate-text
  {:style 
    {:width "75px"
     :height "24px"
     :white-space "nowrap"
     :overflow "hidden"
     :text-overflow "ellipsis"
     :background-color "white"}})

(def drag-default
  {:draggable "true"
   :on-drag #(.preventDefault %)
   :on-drag-over #(.preventDefault %)
   :on-drag-enter #(.preventDefault %)
   :on-drag-leave #(.preventDefault %)
   :on-drag-end #(.preventDefault %)
   :on-drop #(.preventDefault %)
   :on-drag-start #(.preventDefault %)})

;;;

(defn tooltip [string]
  [:span {:class "icon is-pulled-right has-text-grey-light"
          :title string}
         [:i {:class "fas fa-info-circle"}]])
