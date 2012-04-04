(ns shady.gen-class
  "Replacement for `clojure.core/gen-class` which supports dynamic class
redefinition."
  (:refer-clojure :exclude [gen-class]))

(def ^:private core-generate-class
  (deref #'clojure.core/generate-class))

(defn- reload-class
  [pqname bytecode]
  (-> (clojure.lang.RT/baseLoader) (.defineClass pqname bytecode '())))

(defn generate-class
  "Function version of `gen-class`.  Takes a map of options which are otherwise
as for `gen-class`.  Does not import the generated class into the current
namespace."
  [options-map]
  (let [[cname bytecode] (core-generate-class options-map)]
    (if *compile-files*
      (clojure.lang.Compiler/writeClassFile cname bytecode)
      (reload-class (:name options-map) bytecode))))

(defmacro gen-class
  "Augmented version of `clojure.core/gen-class`, taking the same arguments.
When not compiling, defines the class using a dynamic class-loader, allowing
run-time redefinition of the generated class.  Imports the class into the
current namespace."
  [& options]
  (let [options-map (apply hash-map options)]
    (generate-class options-map)
    `(import ~(symbol (:name options-map)))))
