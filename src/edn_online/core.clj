(ns edn-online.core
  (:require [clojure.edn :as edn])
  (:gen-class))

;;a basic program that allows you to edit EDN directly, and tells you the types of EDN objects that you're seeing
;;edn is only dealing with 1 value, so it's either a collection or an atom
;;a : text -> edn
;;b : edn -> types
;;c : edn -> json
;;d : json -> edn
;;


(defmacro validate-inp
  "Takes a fun, and wraps it in a try-catch for runtime exceptions. Will use this to catch poorly formed user input."
  [f]
  `(try
    ~f
    (catch e java.lang.RunTimeException e)))

(defn edn->str [s]
  (pr-str s))

(defn str->edn [s]
  (edn/read-string s))

(declare edn->types)

(defn- edn-map->types
  "{:a b} ->  {[(type :a) :a] [(type b) b]}"
  [e]
  (->> (for [[k v] e]
         {(edn->types k)
          (edn->types v)})
       (into {})))

(defn- edn-vec->types
  "[a] -> [[(type a) a]]"
  [e]
  (mapv edn->types e))

(defn- edn-lis->types
  "'(a) -> '([(type a) a])"
  [e]
  (map edn->types e))

(defn edn->types
  "Returns [(type e) e], is called recursively on collections and maintains their structure in the return value."
  [e]
  (let [et (type e)]
    (cond
      ;;if e is a collection, need to return the appropriate type collection
      (= et clojure.lang.PersistentArrayMap) [et (edn-map->types e)]
      (= et clojure.lang.PersistentVector) [et (edn-vec->types e)]
      (= et clojure.lang.PersistentList) [et (edn-lis->types e)]
      ;;otherwise, it's an atom
      :else [et e])))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
