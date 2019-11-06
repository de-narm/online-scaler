(ns online-scaler.events
  (:require-macros [cljs.core.async.macros :refer [go-loop]])
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :refer [split]]
   [re-frame.core :as re-frame]
   [online-scaler.db :as db]))

;;; Init

(re-frame/reg-event-db
 ::initialize-db
  (fn [_ _]
    db/default-db))

;;; Upload

(re-frame/reg-event-db
 ::set-mv-ctx
  (fn [db [_ result]]
    (assoc-in db [:mv-ctx :file] result)))

(re-frame/reg-fx
  :read
  (fn [file]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader) 
            #(re-frame/dispatch [::set-mv-ctx (-> % .-target .-result)]))
      (.readAsText reader file))))

(re-frame/reg-event-fx
 ::mv-ctx-file-change
  (fn [db [_ file]]
    {:read file
     :db (assoc-in db [:mv-ctx :name] (.-name file))}))

(re-frame/reg-event-db
 ::mv-ctx-header-change
  (fn [db [_ value]]
    (assoc-in db [:mv-ctx :header] value)))

(re-frame/reg-event-db
 ::make-mv-ctx
  (fn [db _]
    (assoc-in db [:mv-ctx :ctx] "hoopa")))
