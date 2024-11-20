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
    (assoc-in toroide [y-wrap x-wrap] valor)))              ;;cambie x por y



;;si la lista esta vacia devuelve 0
(defn desapilar [estado]
  (let [pila (:pila estado)] (if (nil? (peek pila)) (assoc estado :pila (vec '(0))) (assoc estado :pila (pop pila)) ))
  )

;; devuelve una pila con los valores ascii de los elementos


;; funciones de automodificacion
(defn modificar-toroide [estado]
  (let [pila (:pila estado)
        y (peek pila)
        pila (:pila (desapilar estado))
        x (peek pila)
        pila-sin-x-y (:pila (desapilar (desapilar estado)) )
        valor (peek pila-sin-x-y)
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
        valor2 (peek (:pila (desapilar estado)) )
        nueva-pila (conj (pop (pop pila)) (+ valor2 valor1))]
    (assoc estado :pila nueva-pila)))

(defn restar [estado] (let [pila (:pila estado)
                            valor1 (peek pila)
                            valor2 (peek (:pila (desapilar estado)) )
                            nueva-pila (conj (pop (pop pila)) (- valor1 valor2))]
                        (assoc estado :pila nueva-pila)))

(defn multiplicar [estado] (let [pila (:pila estado)
                                 valor1 (peek pila)
                                 valor2 (peek (:pila (desapilar estado)) )
                                 nueva-pila (conj (pop (pop pila)) (* valor1 valor2))]
                             (assoc estado :pila nueva-pila)))

(defn dividir [estado] (let [pila (:pila estado)
                             valor1 (peek pila)
                             valor2 (peek (:pila (desapilar estado)) )
                             nueva-pila (conj (pop (pop pila)) (/ valor1 valor2))]
                         (assoc estado :pila nueva-pila)))

(defn modulo [estado] (let [pila (:pila estado)
                            valor1 (peek pila)
                            valor2 (peek (pop pila))
                            nueva-pila (conj (pop (pop pila)) (- valor1 valor2))]
                        (assoc estado :pila nueva-pila)))

;;funciones logicas
(defn negado [valor] (if (zero? valor) 1 0))                ;;corregir

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
                          0 (assoc estado :direccion "derecha")
                          1 (assoc estado :direccion "izquierda")
                          2 (assoc estado :direccion "arriba")
                          3 (assoc estado :direccion "abajo"))))

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
        nuevo-pc (:PC (interpretar-mov estado))
        dato (get-in toroide [ (second nuevo-pc) (first nuevo-pc)])]
    (if (or (nil? dato) (= dato \"))
      (assoc estado :PC nuevo-pc)
      (modo-cadena (assoc estado :PC nuevo-pc  :pila (conj pila (int dato)) )))))


;;funciones condicionales
(defn if-horizontal [estado]
  (let [pila (:pila estado)
        valor (peek pila)
        nuevo-pila (pop pila)
        nuevo-estado (if (zero? valor)
                       (assoc estado :direccion "derecha")
                       (assoc estado :direccion "izquierda"))]
    (assoc nuevo-estado :pila nuevo-pila)))

(defn if-vertical [estado]
  (let [pila (:pila estado)
        valor (peek pila)
        nuevo-pila (pop pila)
        nuevo-estado (if (zero? valor)
                       (assoc estado :direccion "abajo")
                       (assoc estado :direccion "arriba"))]
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

(defn intercambiar [estado]
  (let [pila (:pila estado)]
    (cond
      (= (count pila) 0) (assoc estado :pila [0 0])
      (= (count pila) 1) (let [valor1 (peek pila)
                               nueva-pila (conj (pop pila) valor1 0)]
                           (assoc estado :pila nueva-pila))
      :else (let [estado-sin-valor1 (desapilar estado)
                  valor1 (peek pila)
                  estado-sin-valor2 (desapilar estado-sin-valor1)
                  valor2 (peek (:pila estado-sin-valor1))
                  nueva-pila (conj (conj (:pila estado-sin-valor2) (if (nil? valor1) 0 valor1)) (if (nil? valor2) 0 valor2))]
              (assoc estado :pila nueva-pila)))))




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

(defn saltar [estado] (interpretar-mov estado))

(defn descartar-valor [estado] (let [pila (:pila estado)] (assoc estado :pila (pop pila))))

;;funciones de interpretacion y recorrido
(defn interpretar-dato [estado]
  (let [toroide (:toroide estado)
        PC (:PC estado)
        dato (char (get-in toroide [(second PC) (first PC)]))
        funciones {\+ (fn [estado] (sumar estado))
                   \- (fn [estado] (restar estado))
                   \* (fn [estado] (multiplicar estado))
                   \/ (fn [estado] (dividir estado))
                   \% (fn [estado] (modulo estado))
                   \! (fn [estado] (negado estado))
                   \' (fn [estado] (mayor estado))
                   \? (fn [estado] (random estado))
                   \_ (fn [estado] (if-horizontal estado))
                   \| (fn [estado] (if-vertical estado))
                   \$ (fn [estado] (desapilar estado))
                   \. (fn [estado] (print-int estado))
                   \# (fn [estado] (saltar estado))
                   \g (fn [estado] (pasar-dato-a-pila estado))
                   \p (fn [estado] (modificar-toroide estado))
                   \~ (fn [estado] (obtener-byte-y-apilar estado))
                   \& (fn [estado] (obtener-numero-y-apilar estado))
                   \: (fn [estado] (dup estado))
                   \\ (fn [estado] (intercambiar estado))
                   \" (fn [estado] (modo-cadena estado))
                   \^ (fn [estado] (assoc estado :direccion "arriba"))
                   \< (fn [estado] (assoc estado :direccion "izquierda"))
                   \> (fn [estado] (assoc estado :direccion "derecha"))
                   \v (fn [estado] (assoc estado :direccion "abajo"))
                   \, (fn [estado] (print-ascii estado))
                   \0 (fn [estado] (let [nueva-pila (conj (:pila estado) 0)] (assoc estado :pila nueva-pila)))
                   \1 (fn [estado] (let [nueva-pila (conj (:pila estado) 1)] (assoc estado :pila nueva-pila)))
                   \2 (fn [estado] (let [nueva-pila (conj (:pila estado) 2)] (assoc estado :pila nueva-pila)))
                   \3 (fn [estado] (let [nueva-pila (conj (:pila estado) 3)] (assoc estado :pila nueva-pila)))
                   \4 (fn [estado] (let [nueva-pila (conj (:pila estado) 4)] (assoc estado :pila nueva-pila)))
                   \5 (fn [estado] (let [nueva-pila (conj (:pila estado) 5)] (assoc estado :pila nueva-pila)))
                   \6 (fn [estado] (let [nueva-pila (conj (:pila estado) 6)] (assoc estado :pila nueva-pila)))
                   \7 (fn [estado] (let [nueva-pila (conj (:pila estado) 7)] (assoc estado :pila nueva-pila)))
                   \8 (fn [estado] (let [nueva-pila (conj (:pila estado) 8)] (assoc estado :pila nueva-pila)))
                   \9 (fn [estado] (let [nueva-pila (conj (:pila estado) 9)] (assoc estado :pila nueva-pila)))}]
    ((get funciones dato (fn [estado] estado)) estado)))


(defn recorrer-toroide [estado]
  (let [toroide (:toroide estado) contador-programa (:PC estado) dato (get-in  toroide [(second contador-programa) (first contador-programa)])]
    (cond
      (= dato \@) nil
      (= dato \space) (recorrer-toroide (interpretar-mov estado))
      :else (recorrer-toroide (interpretar-mov (interpretar-dato estado))))))

(defn crear-estado-inicial []
  {:toroide (crear-toroide)
   :pila []
   :PC [0 0]
   :direccion "derecha"})

(defn ejecutar-programa [toroide]
  (let [estado-inicial (assoc (crear-estado-inicial) :toroide toroide)]
    (recorrer-toroide estado-inicial)))

(defn cargar-programa [ruta-archivo]
(let [lineas (slurp ruta-archivo)
      toroide (crear-toroide)]
  (reduce
    (fn [toroide [fila linea]]
      (reduce
      (fn [toroide [columna caracter]]
        (escribir-celdas toroide columna fila caracter))
      toroide
      (map vector (range) linea)))
    toroide
    (map vector (range) (clojure.string/split-lines lineas)))))

;; Punto de entrada
(defn -main [& args]
  (let [ruta-archivo (first args)
        toroide (cargar-programa ruta-archivo)]
    (ejecutar-programa toroide)))