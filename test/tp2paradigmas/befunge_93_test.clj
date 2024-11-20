(ns tp2paradigmas.befunge-93-test
  (:require [clojure.test :refer :all]
            [tp2paradigmas.core :refer :all]))

(deftest helloWorld
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/hello.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest Chars
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/chars.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest maze
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/maze.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest serp2
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/serp2.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest palinqn
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/palinqn.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest life
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/life.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))

(deftest prime
  (testing "Ejecuta el programa y valida la salida"
    (let [ruta "resources/prime.bf"
          salida (with-out-str (-main ruta)) ;; Captura la salida estándar
          esperado "Hello world!"]          ;; Salida esperada
      (is (= salida esperado) "La salida no coincide con lo esperado"))))


