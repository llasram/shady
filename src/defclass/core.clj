(ns defclass.core
  (:require [clojure.java.io :as io]))

(defn- class-name
  [sym] (.getName ^Class (resolve sym)))

(defn- method-sigs
  [spec]
  (let [mname (first spec)
        spec (drop-while (comp not sequential?) spec)
        maybe-params (first spec)
        meta-tag #(get (meta %) :tag 'void)
        sig-for #(vector mname (->> % rest (map meta-tag) vec) (meta-tag %))]
    (if (vector? maybe-params)
      [(sig-for maybe-params)]
      (map (comp sig-for first) spec))))

(defn- fn-body
  [fields state body]
  (let [[[this :as params] & body] body]
    (if (empty? fields)
      (list* params body)
      (let [fields (if (= 1 (count fields)) (first fields) fields)]
        `(~params (let [~fields (. ~this ~state)] ~@body))))))

(defn- defn-bodies
  [prefix fields state init-name spec]
  (let [mname (first spec)
        fname (with-meta (symbol (str prefix mname)) (meta mname))
        [fmeta bodies] (split-with (comp not sequential?) (rest spec))
        bodies (if (vector? (first bodies)) [bodies] bodies)]
    (if (= mname init-name)
      `(defn ~fname ~@fmeta ~@bodies)
      `(defn ~fname ~@fmeta
         ~@(map (partial fn-body fields state) bodies)))))

(def ^:private generate-class
  (deref (ns-resolve (the-ns 'clojure.core) 'generate-class)))

(defn- reload-class
  [pqname bytecode]
  (-> (clojure.lang.RT/baseLoader) (.defineClass pqname bytecode '())))

(defn- generate-class-reloadable
  [options-map]
  (let [[cname bytecode] (generate-class options-map)]
    (if *compile-files*
      (clojure.lang.Compiler/writeClassFile cname bytecode)
      (reload-class (:name options-map) bytecode))))

(defmacro gen-class-reloadable [& options]
  (let [options-map (apply hash-map options)]
    (generate-class-reloadable options-map)
    `(import ~(symbol (:name options-map)))))

(defmacro ^:private prog1
  "Evaluates the result of expr, then evaluates the forms in body (presumably
for side-effects), then returns the result of expr."
  [expr & body] `(let [value# ~expr] ~@body value#))

(defmacro defclass
  [cname fields & opts+specs]
  (let [[opts specs] (->> (partition-all 2 opts+specs)
                          (split-with (comp keyword? first))
                          (map (partial apply concat)))
        opts (apply hash-map opts)
        specs (first (reduce (fn [[specs iface] form]
                               (if (sequential? form)
                                 [(update-in specs [iface] conj form) iface]
                                 [(assoc specs form []) form]))
                             [{} cname] specs))
        pqname (str *ns* "." cname)
        prefix (str "__" cname "-")
        not-ifaces (hash-set 'Object (:extends opts) cname)
        impl-names (apply hash-set (keys specs))
        implements (->> (apply disj impl-names not-ifaces) (map class-name) vec)
        method-specs (get specs cname [])
        method-map (->> method-specs (reduce #(assoc %1 (first %2) %2) {}))
        init-name (or (:init opts) '-init)
        [method-map opts] (if (contains? method-map init-name)
                            [(dissoc method-map init-name)
                             (assoc opts :init init-name)]
                            [method-map opts])
        methods (->> method-map vals (mapcat method-sigs) vec)
        state (-> (or (:state opts) (str prefix 'state))
                  str (.replace \- \_) symbol)
        opts (if (or (:state opts) (seq fields))
               (assoc opts :state state)
               opts)
        opts (if-let [extends (:extends opts)]
               (assoc opts :extends (.getName (resolve extends)))
               opts)
        opts (assoc opts
               :name pqname
               :implements implements
               :methods methods
               :prefix prefix)]
    (generate-class-reloadable opts)
    `(prog1 (import ~(symbol pqname))
       ~@(map (partial defn-bodies prefix fields state init-name)
              (apply concat (vals specs))))))
