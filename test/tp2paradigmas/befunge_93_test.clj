(ns tp2paradigmas.befunge-93-test
  (:require [clojure.test :refer :all]
            [tp2paradigmas.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
(defn leer-digitos []
  (println "Ingresa un número seguido de un carácter no numérico:")
  (let [entrada (->> (repeatedly #(char (.read System/in))) ;; Leer caracteres uno a uno
                     (take-while #(Character/isDigit (char %) ))    ;; Detenerse en el primer no-dígito
                     (apply str))                          ;; Convertir los dígitos a una cadena
        numero (if (seq entrada) (Integer/parseInt entrada) 0)] ;; Convertir a número, o 0 si no hay entrada
    numero))

(defn obtener-numero-y-apilar [estado]
  (let [numero (leer-digitos)
        pila (:pila estado)]
    (assoc estado :pila (conj pila numero))))

(deftest input2
  (testing "simple"
    (with-in-str "97z" ;; Simula la entrada como si fuera "123abc"
                 (let [estado1 {:direccion "derecha" :pila [0 97 0 1] :PC [2 2] :toroide [[1 2 3] [4 5 6] [7 8 9]]}]
                   (let [estado2 {:direccion "derecha", :pila [0 97 0 1 123], :PC [2 2], :toroide [[1 2 3] [4 5 6] [7 8 9]]}]
                     (is (= (obtener-numero-y-apilar estado1) estado2)))))))
(defn leer-digitos []
  (println "Ingresa un número seguido de un carácter no numérico:")
  (let [entrada (->> (repeatedly #(read)) ;; Usar read para simular la entrada
                     (take-while #(Character/isDigit (char %) ))    ;; Detenerse en el primer no-dígito
                     (apply str))                          ;; Convertir los dígitos a una cadena
        numero (if (seq entrada) (Integer/parseInt entrada) 0)] ;; Convertir a número, o 0 si no hay entrada
    numero))

(defn obtener-numero-y-apilar [estado]
  (let [numero (leer-digitos)
        pila (:pila estado)]
    (assoc estado :pila (conj pila numero))))

;; Test con entrada simulada
(deftest input1
  (testing "simple"
    (with-in-str "97z" ;; Simula la entrada como si fuera "123abc"
                 (let [estado1 {:direccion "derecha" :pila [0 97 0 1] :PC [2 2] :toroide [[1 2 3] [4 5 6] [7 8 9]]}]
                   (let [estado2 {:direccion "derecha", :pila [0 97 0 1 0], :PC [2 2], :toroide [[1 2 3] [4 5 6] [7 8 9]]}]
                     (is (= (obtener-numero-y-apilar estado1) estado2)))))))
