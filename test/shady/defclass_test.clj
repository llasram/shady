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

