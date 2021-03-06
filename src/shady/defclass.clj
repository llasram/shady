(ns shady.defclass
  "Provide a `defclass` form implementing a `deftype`-like interface for
producing `gen-class`-backed JVM interop classes."
  (:use [shady.gen-class :only [generate-class]]))

(defn- class-name
  [sym] (.getName ^Class (resolve sym)))

(defn- method-sigs
  [spec]
  (let [mname (first spec)
        static (:static (meta mname))
        spec (drop-while (comp not sequential?) spec)
        maybe-params (first spec)
        meta-tag #(get (meta %) :tag 'Object)
        sig-for (fn [margs]
                  (with-meta
                    [mname (->> margs rest (map meta-tag) vec) (meta-tag margs)]
                    {:static static}))]
    (if (vector? maybe-params)
      [(sig-for maybe-params)]
      (map (comp sig-for first) spec))))

(defn- assoc-meta
  [obj key val] (with-meta obj (assoc (meta obj) key val)))

(defn- fn-body-instance
  [pqname fields state params body]
  (let [[this & args] params, this (assoc-meta this :tag pqname)
        params (with-meta (apply vector this args) (meta params))]
    (if (nil? fields)
      (list* params body)
      `(~params (let [~fields (. ~this ~state)] ~@body)))))

(defn- fn-body
  [pqname fname fields state body]
  (let [[params & body] body]
    (if (:static (meta fname))
      (list* params body)
      (fn-body-instance pqname fields state params body))))

(defn- defn-bodies
  [pqname prefix fields state init-name spec]
  (let [mname (first spec)
        fname (with-meta (symbol (str prefix mname)) (meta mname))
        [fmeta bodies] (split-with (comp not sequential?) (rest spec))
        bodies (if (vector? (first bodies)) [bodies] bodies)]
    (if (= mname init-name)
      `(defn ~fname ~@fmeta ~@bodies)
      `(defn ~fname ~@fmeta
         ~@(map (partial fn-body pqname fname fields state) bodies)))))

(defmacro defclass
  "(defclass name [fields*] options* specs*)

Creates a class named `name` in the package of the current namespace and
imports that class into the current namespace.  When AOT-compiling, writes the
generated class-file to the appropriate package-location under
`*compile-path*`.

The `options` are as per the options to `clojure.core/gen-class`, with the
following differences: (a) the `:name` and `:methods` arguments are replaced by
the syntax of `defclass`; (b) the `:impl-ns` option may not be provided,
and (c) the following new options are available:

- `:package`: The name of the package in which to create the new class.
  Defaults to the package name of the current namespace.

Each of the `specs` consists of a class/interface name followed by zero or more
method bodies:

    ClassOrInterface
    (methodName [args*] body)*

`ClassOrInterface` is limited to: (a) Object; (b) the superclass specified with
the `:extends` option; (c) the class being implemented, which will add new
methods to the class; or (d) an arbitrary interface, which will add the
interface to the set of interfaces the class implements.  Any method bodies
which occur prior to the first `ClassOrInterface` symbol will be used as new
methods of the generated class, as per case (c).

Implementations may provide a `gen-class` style initialization function by
including an own-class method named either `-init` or what is provided as the
value of the `:init` option.

If any `fields` are provided, then the class will automatically use the
`gen-class` state feature to store the field values in a vector.  The name of
the state field may be specified with the `:state` option, or a default will be
generated.  Within method bodies, the instance values of the fields will be
automatically bound to locals of the same names, similar to the field behavior
for `deftype`.  Unlike for `deftype`, these \"fields\" cannot be made mutable.
As with direct use of the `gen-class` state feature, utilizing classes must
provide an initialization function."
  [name [& fields] & opts+specs]
  (let [[opts specs] (->> (partition-all 2 opts+specs)
                          (split-with (comp keyword? first))
                          (map (partial apply concat)))
        opts (apply hash-map opts)
        specs (first (reduce (fn [[specs iface] form]
                               (if (sequential? form)
                                 [(update-in specs [iface] conj form) iface]
                                 [(assoc specs form []) form]))
                             [{} name] specs))
        [package opts] [(or (:package opts) (namespace-munge *ns*))
                        (dissoc opts :package)]
        pqname (symbol (str package "." name))
        prefix (or (:prefix opts) (str "__" name "-"))
        not-ifaces (hash-set 'Object (:extends opts) name)
        impl-names (apply hash-set (keys specs))
        implements (->> (apply disj impl-names not-ifaces) (map class-name) vec)
        method-specs (get specs name [])
        method-map (->> method-specs (reduce #(assoc %1 (first %2) %2) {}))
        init-name (or (:init opts) '-init)
        [method-map opts] (if (contains? method-map init-name)
                            [(dissoc method-map init-name)
                             (assoc opts :init init-name)]
                            [method-map opts])
        methods (->> method-map vals (mapcat method-sigs) vec)
        fields (if (>= 1 (count fields)) (first fields) (vec fields))
        state (-> (or (:state opts) (str prefix 'state))
                  str (.replace \- \_) symbol)
        opts (if (or (:state opts) fields)
               (assoc opts :state state)
               opts)
        opts (if-let [extends (:extends opts)]
               (assoc opts :extends (class-name extends))
               opts)
        opts (assoc opts
               :name (str pqname)
               :implements implements
               :methods methods
               :prefix prefix)]
    (generate-class opts)
    `(let [result# (import ~pqname)]
       ~@(map (partial defn-bodies pqname prefix fields state init-name)
              (apply concat (vals specs)))
       result#)))
