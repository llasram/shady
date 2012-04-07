# Shady

A collection of inter-operators for Clojure on the JVM.  Designed to make your
interactions with Java as smooth as possible.

## Installation

Shady is available on Clojars.  Add this `:dependency` to your Leiningen
`project.clj`:

```clj
[shady "0.1.1"]
```

## Usage

At present includes two useful bits of functionality: a version of `gen-class`
supporting dynamic redefinition like `deftype`; and a `defclass` macro
providing a `deftype`-like interface to that `gen-class`.  For example:

```clj
(ns example.defclass
  (:use [shady.defclass :only [defclass]]))

(defclass Example [field1 field2]
  :extend BaseClass
  :constructors {[String String] [String]}

  Example
  (-init [arg1 arg2] [[arg1] [arg1 arg2]])
  (newMethod ^String [^int param]
    (str "example:" field1 ":" param ":" field2))

  BaseClass
  (override ^Long [^Class class] 10)

  SomeInterface
  (implementedMethod []
    "Types defaulting to Object, as you'd hope."))
```

## License

Copyright © 2012 Marshall T. Vandegrift <llasram@gmail.com>

Distributed under the Eclipse Public License, the same as Clojure.
