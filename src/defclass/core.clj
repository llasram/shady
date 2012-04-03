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

(defn- defn-bodies
  [prefix spec]
  (let [mname (first spec)
        fname (with-meta (symbol (str prefix mname)) (meta mname))]
    (list* `defn fname (rest spec))))

(def ^:private generate-class
  (deref (ns-resolve (the-ns 'clojure.core) 'generate-class)))

(defn- reload-class
  [pqname bytecode]
  (doto (clojure.lang.RT/baseLoader)
    (.defineClass pqname bytecode '())))

(defn- generate-reload-class
  [pqname options-map]
  (let [[cname bytecode] (generate-class options-map)]
    (if *compile-files*
      (clojure.lang.Compiler/writeClassFile cname bytecode)
      (reload-class pqname bytecode))))

(defmacro defclass
  [cname [& fields] & opts+specs]
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
        not-ifaces (hash-set 'Object (get opts :extends) cname)
        impl-names (apply hash-set (keys specs))
        implements (->> (apply disj impl-names not-ifaces) (map class-name) vec)
        methods (->> (get specs cname []) (mapcat method-sigs) vec)
        opts (assoc opts
               :name pqname
               :implements implements
               :methods methods
               :prefix prefix)]
    (generate-reload-class pqname opts)
    `(let [result# (import ~(symbol pqname))]
       ~@(map (partial defn-bodies prefix) (apply concat (vals specs)))
       result#)))
