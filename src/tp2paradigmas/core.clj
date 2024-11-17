(ns tp2paradigmas.core)

(def ancho 80)
(def alto 25)
;; funcion para crear una grilla 80x25 con espacios
(defn crear-toroide []
  (vec (repeat alto (vec (repeat ancho \space)))))

(defn envolver-coord [x max]
  (mod x max))

;; funcion que envuelve las coordenadas si se pasan de los limites de la grilla
(defn envolver-coords [x y]
  [(envolver-coord x ancho) (envolver-coord y alto)])

;; Lee el valor en una posicion especifica, usando las coordenadas del toroide
(defn leer-celda [toroide x y]
  (let [[x-wrap y-wrap] (envolver-coords x y)]
    (get-in toroide [y-wrap x-wrap])))

(defn escribir-celdas [toroide x y valor]
  (let [[x-wrap y-wrap] (envolver-coords x y)]
    (assoc-in toroide [y-wrap x-wrap] valor)))



;;si la lista esta vacia devuelve 0
(defn desapilar [estado]
  (let [pila (:pila estado)] (if (nil? (peek pila)) 0 (pop pila)))
  )

;; devuelve una pila con los valores ascii de los elementos


;; funciones de automodificacion
(defn modificar-toroide [estado]
  (let [pila (:pila estado)
        y (peek pila)
        pila (pop pila)
        x (peek pila)
        pila (pop pila)
        valor (peek pila)
        pila (pop pila)
        toroide (:toroide estado)
        filas (count toroide)
        columnas (count (first toroide))
        x-envuelto (mod x columnas)
        y-envuelto (mod y filas)
        nuevo-toroide (assoc-in toroide [x-envuelto y-envuelto] (char valor))]
    (assoc estado :pila pila :toroide nuevo-toroide)))

(defn pasar-dato-a-pila [estado]
  (let [pila (:pila estado)
        y (peek pila)
        pila (pop pila)
        x (peek pila)
        pila (pop pila)
        toroide (:toroide estado)
        filas (count toroide)
        columnas (count (first toroide))
        x-envuelto (mod x columnas)
        y-envuelto (mod y filas)
        valor (int (get-in toroide [y-envuelto x-envuelto]))]
    (assoc estado :pila (conj pila valor))))

;;funciones de calculo basico
(defn sumar [estado]
  (let [pila (:pila estado)
        valor1 (peek pila)
        valor2 (peek (pop pila))
        nueva-pila (conj (pop (pop pila)) (+ valor1 valor2))]
    (assoc estado :pila nueva-pila)))

(defn restar [estado] (let [pila (:pila estado)
                            valor1 (peek pila)
                            valor2 (peek (pop pila))
                            nueva-pila (conj (pop (pop pila)) (- valor1 valor2))]
                        (assoc estado :pila nueva-pila)))

(defn multiplicar [estado] (let [pila (:pila estado)
                                 valor1 (peek pila)
                                 valor2 (peek (pop pila))
                                 nueva-pila (conj (pop (pop pila)) (* valor1 valor2))]
                             (assoc estado :pila nueva-pila)))

(defn dividir [estado] (let [pila (:pila estado)
                             valor1 (peek pila)
                             valor2 (peek (pop pila))
                             nueva-pila (conj (pop (pop pila)) (/ valor1 valor2))]
                         (assoc estado :pila nueva-pila)))

(defn modulo [estado] (let [pila (:pila estado)
                            valor1 (peek pila)
                            valor2 (peek (pop pila))
                            nueva-pila (conj (pop (pop pila)) (- valor1 valor2))]
                        (assoc estado :pila nueva-pila)))

;;funciones logicas
(defn negado [valor] (if (zero? valor) 1 0))

(defn mayor [estado] (let [pila (:pila estado)
                           valor1 (peek pila)
                           valor2 (peek (pop pila))]
                       (if (> valor1 valor2)
                         (assoc estado :pila (conj (pop (pop pila)) 1))
                         (assoc estado :pila (conj (pop (pop pila)) 0)))))

;;funciones de movimiento
(defn derecha [estado] (let [contador-programa (:PC estado)] (assoc estado :PC [(+ (first contador-programa) 1) (second contador-programa)])))
(defn izquierda [estado] (let [contador-programa (:PC estado)] (assoc estado :PC [(- (first contador-programa) 1) (second contador-programa)])))
(defn abajo [estado] (let [contador-programa (:PC estado)] (assoc estado :PC [(first contador-programa) (+ (second contador-programa) 1)])))
(defn arriba [estado] (let [contador-programa (:PC estado)] (assoc estado :PC [(first contador-programa) (- (second contador-programa) 1)])))
(defn random [estado] (let [eleccion-random (rand-int 4)]
                        (case eleccion-random
                          0 (derecha estado)
                          1 (izquierda estado)
                          2 (arriba estado)
                          3 (abajo estado))))

(defn interpretar-mov [estado] (let [direccion (:direccion estado)]
                                 (case direccion
                                   "izquierda" (izquierda estado)
                                   "derecha" (derecha estado)
                                   "abajo" (abajo estado)
                                   "arriba" (arriba estado))))

;; modo cadena
(defn modo-cadena [estado]
  (let [toroide (:toroide estado)
        pila (:pila estado)
        posicion (:PC estado)
        dato (get toroide posicion)
        nuevo-pc (:PC (interpretar-mov estado))]
    (if (or (nil? dato) (= dato \"))
      pila
      (modo-cadena (assoc estado :PC nuevo-pc  :pila (conj pila (int dato)) )))))


;;funciones condicionales
(defn if-horizontal [estado]
  (let [pila (:pila estado)
        valor (peek pila)
        nuevo-pila (pop pila)
        nuevo-estado (if (= valor 0)
                       (assoc estado :direccion "derecha")
                       (assoc estado :direccion "izquierda"))]
    (assoc nuevo-estado :pila nuevo-pila)))

(defn if-vertical [estado]
  (let [pila (:pila estado)
        valor (peek pila)
        nuevo-pila (pop pila)
        nuevo-estado (if (= valor 0)
                       (assoc estado :direcccion "arriba")
                       (assoc estado :direccion "abajo"))]
    (assoc nuevo-estado :pila nuevo-pila)))

;;funciones de entrada
(defn leer-digitos []
  (println "Ingresa un número seguido de un carácter no numérico:")
  (let [entrada (->> (read-line)
                     (take-while #(Character/isDigit (char %) ))
                     (apply str))
        numero (if (seq entrada) (Integer/parseInt entrada) 0)]
    numero))

(defn obtener-numero-y-apilar [estado]
  (let [numero (leer-digitos)
        pila (:pila estado)]
    (assoc estado :pila (conj pila numero))))

(defn leer-byte []
  (println "Ingresa un carácter:")
  (let [caracter (read-line)]
    (first caracter)))

(defn obtener-byte-y-apilar [estado]
  (let [caracter (leer-byte)]
    (println "Carácter leído:" caracter)
    (let [byte (int caracter)]
      (println "Valor numérico del carácter:" byte)
      (let [pila (:pila estado)]
        (assoc estado :pila (conj pila byte))))))

(defn dup [estado] (let [pila (:pila estado)] (assoc estado :pila (conj pila (peek pila)))))

(defn intercambiar [estado] (let [pila (:pila estado) valor1 (peek pila) valor2 (peek (desapilar pila))]
                              (assoc estado :pila (conj (conj (pop (pop pila)) valor1) valor2))))

;;funciones de salida
(defn print-int [estado]
  (let [pila (:pila estado)
        valor (peek pila)]
    (if (nil? valor)
      estado
      (do
        (print (str valor " "))
        (assoc estado :pila (pop pila))))))

(defn print-ascii [estado]
  (let [pila (:pila estado)
        valor (peek pila)]
    (if (nil? valor)
      estado
      (do
        (print (char valor))
        (assoc estado :pila (pop pila))))))

(defn saltar [estado] (interpretar-mov (interpretar-mov estado)))

(defn descartar-valor [estado] (let [pila (:pila estado)] (assoc estado :pila (pop pila))))

;;funciones de interpretacion y recorrido
(defn interpretar-dato [estado]
  (let [toroide (:toroide estado) PC (:PC estado) dato (char (get-in toroide PC))
        funciones {\+ (sumar estado) \- (restar estado) \* (multiplicar estado) \/ (dividir estado) \% (modulo estado) \! (negado estado)
                   \' (mayor estado) \? (random estado) \_ (if-horizontal estado) \| (if-vertical estado) \$ (desapilar estado) \. (print-int estado)
                   \# (saltar estado) \g (pasar-dato-a-pila estado) \p (modificar-toroide estado) \~ (obtener-byte-y-apilar estado) \& (obtener-numero-y-apilar estado) \: (dup estado)
                   \\ (intercambiar estado) \" (modo-cadena estado) \^ ((fn [estado] (assoc estado :direccion "arriba"))) \< ((fn [estado] (assoc estado :direccion "izquierda")))
                   \> (fn [estado] (assoc estado :direccion "derecha")) \v (fn [estado] (assoc estado :direccion "abajo")) \, (print-ascii estado)
                   \0 (fn [estado] (let [nueva-pila (conj (:pila estado) 0)] (assoc estado :pila nueva-pila)))
                   \1 (fn [estado] (let [nueva-pila (conj (:pila estado) 1)] (assoc estado :pila nueva-pila)))
                   \2 (fn [estado] (let [nueva-pila (conj (:pila estado) 2)] (assoc estado :pila nueva-pila)))
                   \3 (fn [estado] (let [nueva-pila (conj (:pila estado) 3)] (assoc estado :pila nueva-pila)))
                   \4 (fn [estado] (let [nueva-pila (conj (:pila estado) 4)] (assoc estado :pila nueva-pila)))
                   \5 (fn [estado] (let [nueva-pila (conj (:pila estado) 5)] (assoc estado :pila nueva-pila)))
                   \6 (fn [estado] (let [nueva-pila (conj (:pila estado) 6)] (assoc estado :pila nueva-pila)))
                   \7 (fn [estado] (let [nueva-pila (conj (:pila estado) 7)] (assoc estado :pila nueva-pila)))
                   \8 (fn [estado] (let [nueva-pila (conj (:pila estado) 8)] (assoc estado :pila nueva-pila)))
                   \9 (fn [estado] (let [nueva-pila (conj (:pila estado) 9)] (assoc estado :pila nueva-pila)))
                   }]
    ((get funciones dato) estado)))

(defn recorrer-toroide [estado]
  (let [toroide (:toroide estado) contador-programa (:PC estado) dato (get toroide contador-programa)]
    (if (= (dato) \space)
      (recorrer-toroide (interpretar-mov estado))
      ((if (= (dato) \@) nil (recorrer-toroide (interpretar-mov (interpretar-dato estado))))))
    )
  )

;; Punto de entrada
(defn -main [& args]
  (let [ruta-archivo (first args)
        toroide (cargar-programa ruta-archivo)]
    (ejecutar-programa toroide)))