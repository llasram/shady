(ns shady.defclass-test
  (:use [clojure.test])
  (:use [shady.defclass :only [defclass]]))

(defclass Definition []
  (exampleMethod [this] :example))

(deftest test-definition
   (testing "defclass can define classes"
     (is (= :example (.exampleMethod (Definition.))))))


(defclass Redefinition []
  (exampleMethod [this] :first))

(defclass Redefinition []
  (exampleMethod [this] :second))

(deftest test-redefinition
  (testing "defclass can re-define classes"
    (is (= :second (.exampleMethod (Redefinition.))))))


(defclass ExtendBase []
  (exampleMethod [this] :base))

(defclass ExtendSubclass []
  :extends ExtendBase
  ExtendBase
  (exampleMethod [this] :subclass))

(deftest test-extend
  (testing "defclass can extend a superclass"
    (is (= :base (.exampleMethod (ExtendBase.))))
    (is (= :subclass (.exampleMethod (ExtendSubclass.))))
    (is (instance? ExtendBase (ExtendSubclass.)))))


(defclass InitDefault []
  :state datum
  :constructors {[Object] []}
  (-init [datum] [[] datum]))

(defclass InitCustom []
  :state datum
  :constructors {[Object] []}
  :init -init-datum
  (-init-datum [datum] [[] datum]))

(deftest test-init
  (testing "defclass can use an initializer function"
    (is (= :default (.datum (InitDefault. :default))) "with the default name")
    (is (= :custom (.datum (InitDefault. :custom))) "with a custom name")))


(defclass Fields [field-a field-b]
  :state state
  :constructors {[Object Object] []}
  (-init [field-a field-b] [[] [field-a field-b]])
  (fieldA [this] field-a)
  (fieldB [this] field-b))

(deftest test-fields
  (testing "Field values are locally accessible in methods"
    (let [obj (Fields. :left :right)]
      (is (= :left (.fieldA obj)))
      (is (= :right (.fieldB obj))))))


(defclass SingleField [field]
  :constructors {[Object] []}
  (-init [arg] [[] arg])
  (getField [this] field))

(deftest test-single-field
  (testing "defclass can define classes with a single field"
    (let [obj (SingleField. :works)]
      (is (= :works (.getField obj))))))


(defclass Static []
  (^:static staticMethod [] :works))

(deftest test-static
  (testing "defclass can define static methods"
    (is (= :works (Static/staticMethod)))))


(defclass Identical []
  (exampleMethod [this] :works))

(deftest test-identical []
  (testing "defclass classes are identical across expressions"
    (let [^Identical obj (Identical.)]
      (is (= Identical (class (Identical.))))
      (is (= :works (.exampleMethod obj))))))


(defclass OtherPackage []
  :package other.package
  (exampleMethod [this] :works))

(deftest test-other-package []
  (testing "defclass can create classes in other packages"
    (let [obj (other.package.OtherPackage.)]
      (is (= :works (.exampleMethod obj))))))
