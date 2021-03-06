(ns online-scaler.subs
  (:require 
   [re-frame.core :as re-frame]))

;;;-Upload---------------------------------------------------------------------

(re-frame/reg-sub
 ::mv-ctx-file-name
  (fn [db _]
    (get-in db [:mv-ctx :name])))

(re-frame/reg-sub
 ::mv-ctx-file
  (fn [db _]
    (get-in db [:mv-ctx :file])))

(re-frame/reg-sub
 ::header-checkbox
  (fn [db _]
    (get-in db [:mv-ctx :header])))

(re-frame/reg-sub
 ::objects-checkbox
  (fn [db _]
    (get-in db [:mv-ctx :has-names])))

(re-frame/reg-sub
 ::mv-ctx-separator
  (fn [db _]
    (get-in db [:mv-ctx :separator])))

(re-frame/reg-sub
 ::mv-ctx-custom-separator
  (fn [db _]
    (get-in db [:mv-ctx :custom])))

;;;-Import---------------------------------------------------------------------

(re-frame/reg-sub
 ::import-name
  (fn [db _]
    (get-in db [:db :name])))

(re-frame/reg-sub
 ::import-db
  (fn [db _]
    (get-in db [:db :map])))

;;;-Selection------------------------------------------------------------------

(re-frame/reg-sub
 ::ctx-attributes
  (fn [db _]
    (get-in db [:ctx :attributes])))

(re-frame/reg-sub
 ::ctx-values
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :values])))

;;;-Ordinal-Scaling-Drag-------------------------------------------------------

(re-frame/reg-sub
 ::context-view
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :context-view])))

(re-frame/reg-sub
 ::current-distinct
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :distinct])))

(re-frame/reg-sub
 ::orders
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :orders])))

;;;-Ordinal-Scaling-Context----------------------------------------------------

(re-frame/reg-sub
 ::current-attributes
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :attributes])))

(re-frame/reg-sub
 ::incidence
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :incidence])))

(re-frame/reg-sub
 ::relation
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :relation-name])))

;;;-Numeric-Scaling------------------------------------------------------------

(re-frame/reg-sub
 ::selected-attributes
  (fn [db [_ [attribute listkey]]]
    (get-in db [:scaling (keyword attribute) listkey])))

(re-frame/reg-sub
 ::multiple-names
  (fn [db [_ [namestring listkey]]]
    (let [current-attribute (get-in db [:selection :current-attribute])
          attributes (get-in db 
                             [:scaling (keyword current-attribute) listkey])
          names      (map :name attributes)]
      (if (= 1 (count (filter #{namestring} names))) false true))))

;;;-Scaling--------------------------------------------------------------------

(re-frame/reg-sub
 ::current-attribute
  (fn [db _]
    (get-in db [:selection :current-attribute])))

(re-frame/reg-sub
 ::attribute-list
  (fn [db _]
    (get-in db [:selection :attributes])))

(re-frame/reg-sub
 ::attribute-measure
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :measure])))

(re-frame/reg-sub
 ::attribute-numerical
  (fn [db [_ attribute]]
    (get-in db [:scaling (keyword attribute) :numerical])))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-sub
 ::context-url
  (fn [db _]
    (get-in db [:context-url])))

(re-frame/reg-sub
 ::config-url
  (fn [db _]
    (get-in db [:config-url])))

(re-frame/reg-sub
 ::warning
  (fn [db _]
    (get-in db [:warning])))

(re-frame/reg-sub
 ::ctx-file-name
  (fn [db _]
    (get-in db [:ctx :name])))

;;;-Panel----------------------------------------------------------------------

(re-frame/reg-sub
 ::panel
  (fn [db _]
    (get-in db [:panel])))

;;;-Util-----------------------------------------------------------------------

(re-frame/reg-sub
 ::tmp
  (fn [db _]
    (get-in db [:tmp])))
