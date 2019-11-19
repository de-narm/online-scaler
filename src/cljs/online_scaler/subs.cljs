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

;;;-Selection------------------------------------------------------------------

(re-frame/reg-sub
 ::mv-ctx-attributes
  (fn [db _]
    (get-in db [:mv-ctx :attributes])))

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
    (get-in db [:scaling (first attribute) :measure])))

;;;-Export---------------------------------------------------------------------

(re-frame/reg-sub
 ::url
  (fn [db _]
    (get-in db [:url])))

;;;-Panel----------------------------------------------------------------------

(re-frame/reg-sub
 ::panel
  (fn [db _]
    (get-in db [:panel])))

;;;-Util-----------------------------------------------------------------------

(re-frame/reg-sub
 ::get-tmp
  (fn [db _]
    (get-in db [:tmp])))
