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
  (newMethod ^String [this ^int param]
    (str "example:" field1 ":" param ":" field2))
  (defaultTypes [this arg1 arg2]
    "Types defaulting to Object, as you'd hope.")
  (^:static staticMethod [arg1 arg2]
    "Defining static methods is just as easy.")

  BaseClass
  (override [this obj] 10)

  SomeInterface
  (implementedMethod [this]
    "Implement all the interfaces!"))
```

## License

Copyright © 2012 Marshall T. Vandegrift <llasram@gmail.com>

Distributed under the Eclipse Public License, the same as Clojure.
