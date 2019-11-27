(ns online-scaler.events
  (:require
   [cljs.reader :refer [read-string]]
   [clojure.string :refer [split]]
   [re-frame.core :as re-frame]
   [online-scaler.scaling :as scale]
   [online-scaler.db :as db]))

;;;-Init-----------------------------------------------------------------------

(re-frame/reg-event-db
 ::initialize-db
  (fn [_ _]
    db/default-db))

;;;-Upload---------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-mv-ctx 
  (fn [db [_ result]]
    ;; Save file as [["First" "line" "in" ".csv"] ["Second" "line"]]
    (assoc-in db [:mv-ctx :file] (map #(split % #", |,") 
                                      (split result #"\n")))))

(re-frame/reg-fx
  :read-mv-ctx
   (fn [file]
     (let [reader (js/FileReader.)]
       (set! (.-onload reader) 
             #(re-frame/dispatch [::set-mv-ctx (-> % .-target .-result)]))
       (.readAsText reader file))))

(re-frame/reg-event-fx 
 ::mv-ctx-file-change
  (fn [cofx [_ file]]
    {:read-mv-ctx file
     :db (assoc-in (:db cofx) [:mv-ctx :name] (.-name file))}))

(re-frame/reg-event-db
 ::mv-ctx-header-change
  (fn [db [_ value]]
    (assoc-in db [:mv-ctx :header] value)))

(re-frame/reg-event-fx
 ::initiate-selection
  (fn [cofx _]
    {:dispatch [::set-panel "select"]
     :db  (let [db                (:db cofx)
                header            (get-in db [:mv-ctx :header])
                mv-ctx            (get-in db [:mv-ctx :file])
                first-line        (first mv-ctx)
                attribute-vector  (if header
                                      ;; either use given names
                                      (mapv 
                                        #(vector (str %) true) 
                                        first-line)
                                      ;; or generate generic ones
                                      (mapv
                                        #(vector (str "Attribute#" %) true)
                                        (range (count first-line))))
                ;; transpose [[1 1][2 2]] -> [[1 2][1 2]]
                new-mv-ctx        (apply map list
                                    (if header (drop 1 mv-ctx) mv-ctx))]
          (-> db
            (assoc-in [:mv-ctx :file] new-mv-ctx)
            (assoc-in [:mv-ctx :attributes] attribute-vector)
            (assoc-in [:scaling] (apply merge
                                    (map 
                                      #(hash-map 
                                         (keyword %)
                                         (hash-map :measure "nominal")) 
                                      (map first attribute-vector))))))}))

;;;-Import---------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-import-db
  (fn [db [_ new-db]]
    (assoc-in db [:db :map] new-db)))

(re-frame/reg-fx
 :read-import
  (fn [file]
    (let [reader (js/FileReader.)]
      (set! (.-onload reader) 
            #(re-frame/dispatch [::set-import-db (js->clj
                                                   (->> % 
                                                        .-target 
                                                        .-result
                                                        (.parse js/JSON))
                                                   :keywordize-keys true)]))
      (.readAsText reader file))))

(re-frame/reg-event-fx 
 ::import-file-change
  (fn [cofx [_ file]]
    {:read-import file
     :db (assoc-in (:db cofx) [:db :name] (.-name file))}))

(re-frame/reg-event-db
 ::set-db 
  (fn [db _]
    (if (nil? (get-in db [:db :map]))
      db
      (get-in db [:db :map]))))

;;;-Selection------------------------------------------------------------------

(re-frame/reg-event-fx
 ::attribute-selection-down
  (fn [cofx [_ attribute]]
    (let [db    (:db cofx) 
          index (.indexOf (get-in db [:mv-ctx :attributes]) attribute)]
      {:dispatch [::set-tmp index]})))

(re-frame/reg-event-db
 ::attribute-selection-up
  (fn [db [_ attribute]]
    (let [down-index (:tmp db) 
          up-index   (.indexOf (get-in db [:mv-ctx :attributes]) attribute)
          index-list (if (> down-index up-index)
                         (conj (range up-index down-index) down-index)
                         (conj (range down-index up-index) up-index))]
      ;; go through all indices and swap if they are selected
      (loop [loop-db          (assoc-in db [:tmp] nil)
             loop-index-list  index-list]
        (if (empty? loop-index-list)
            ;; empty the tmp value
            (assoc-in loop-db [:tmp] nil)
            (recur
              (update-in 
                loop-db 
                [:mv-ctx :attributes (first loop-index-list) 1]
                not)
              (drop 1 loop-index-list)))))))

(re-frame/reg-event-fx
 ::initiate-attributes
  (fn [cofx _]
    {:dispatch [::set-panel "scale"]
     :db (let [db             (:db cofx)
               attribute-list (mapv 
                                first
                                (filter
                                  #(= (second %) true)
                                  (get-in db [:mv-ctx :attributes])))]
           (-> db
             (assoc-in [:selection :attributes] attribute-list)
             (assoc-in 
               [:selection :current-attribute] 
               (first attribute-list))))}))

;;;-Scaling--------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-current-attribute
  (fn [db [_ attribute]]
    (assoc-in db [:selection :current-attribute] attribute)))

(re-frame/reg-event-db
 ::next-attribute
  (fn [db _]
    (let [current-attribute     (get-in db [:selection :current-attribute])
          attributes            (get-in db [:selection :attributes])
          next-position         (mod
                                 (+ (.indexOf attributes current-attribute) 1)
                                 (count attributes))
          next-attribute        (nth attributes next-position)]
      (assoc-in db [:selection :current-attribute] next-attribute))))

(re-frame/reg-event-db
 ::previous-attribute
  (fn [db _]
    (let [current-attribute     (get-in db [:selection :current-attribute])
          attributes            (get-in db [:selection :attributes])
          next-position         (mod
                                 (- (.indexOf attributes current-attribute) 1)
                                 (count attributes))
          next-attribute        (nth attributes next-position)]
      (assoc-in db [:selection :current-attribute] next-attribute))))

(re-frame/reg-event-db
 ::change-measure
  (fn [db [_ measure]]
    (let [attribute (get-in db [:selection :current-attribute])]
      (assoc-in db [:scaling (keyword attribute) :measure] measure))))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-event-db
 ::remove-export-warning
  (fn [db _]
    (assoc-in db [:warning] false)))

(re-frame/reg-event-fx
 ::make-context
  (fn [cofx _]
    {:dispatch [::remove-export-warning nil]
     :db
       (let [db           (:db cofx)
             context-blob (js/Blob. [(scale/write db)])
             context-url  (js/URL.createObjectURL context-blob)]
         (assoc-in db [:context-url]  context-url))}))

(re-frame/reg-event-fx
 ::make-config
  (fn [cofx _]
    {:dispatch [::remove-export-warning nil]
     :db
       (let [;; remove urls since the files are not included
             db           (:db cofx)
             new-db       (-> db
                            (assoc-in [:warning] false)
                            (assoc-in [:panel] "select")
                            (assoc-in [:context-url] nil)
                            (assoc-in [:config-url] nil))
             config-blob  (js/Blob. [(.stringify js/JSON (clj->js new-db))])
             config-url   (js/URL.createObjectURL config-blob)]
         (assoc-in db [:config-url]  config-url))}))

(re-frame/reg-event-fx
 ::set-export-warning
  (fn [cofx _]
    {:dispatch [::set-panel "export"]
     :db       (assoc-in (:db cofx) [:warning] true)}))

;;;-Panel----------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-panel
  (fn [db [_ panel]]
    (assoc-in db [:panel] panel)))

;;;-Util-----------------------------------------------------------------------

(re-frame/reg-event-db
 ::set-tmp
  (fn [db [_ value]]
    (assoc-in db [:tmp] value)))
