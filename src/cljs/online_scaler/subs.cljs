(ns online-scaler.subs
  (:require 
   [re-frame.core :as re-frame]))

;;; Upload

(re-frame/reg-sub
 ::mv-ctx-file-name
  (fn [db _]
    (get-in db [:mv-ctx :name])))

(re-frame/reg-sub
 ::mv-ctx
  (fn [db _]
    (get-in db [:mv-ctx :ctx])))
