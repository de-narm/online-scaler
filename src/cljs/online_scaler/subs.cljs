(ns online-scaler.subs
  (:require 
   [re-frame.core :as re-frame]))

;;; Upload

(re-frame/reg-sub
 ::mv-ctx-file-name
  (fn [db _]
    (get-in db [:mv-ctx :name])))

(re-frame/reg-sub
 ::mv-ctx-file
  (fn [db _]
    (get-in db [:mv-ctx :file])))

;;; Selection

(re-frame/reg-sub
 ::mv-ctx-attributes
  (fn [db _]
    (get-in db [:mv-ctx :attributes])))

;;; Export

(re-frame/reg-sub
 ::url
  (fn [db _]
    (get-in db [:url])))

;;; Panel

(re-frame/reg-sub
 ::panel
  (fn [db _]
    (get-in db [:panel])))
