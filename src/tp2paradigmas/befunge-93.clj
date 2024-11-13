(def width 80)
(def height 25)

;; funcion para crear una grilla 80x25 con espacios
(defn crear-toroide []
  (vec (repeat height (vec (repeat width \space)))))

(defn envolver-coord [x max]
  (mod x max))

;; funcion que envuelve las coordenadas si se pasan de los limites de la grilla
(defn envolver-coords [x y]
  [(wrap-coord x width) (wrap-coord y height)])

;;devuelve la nueva lista sin el ultimo dato agregado (LIFO)
;;si la lista esta vacia devuelve 0
(defn desapilar [pila]
  (if (nil? (peek pila)) 0 (pop pila)))

;apila los comandos del 0 al 9
(defn comandos-0-9 [comando pila]
  (if (>= comando 0) (if (<= comando 9) (conj comando pila))) )

;; devuelve una pila con los valores ascii de los elementos
;; apilados hasta la comilla final.
(defn modo-cadena [posicion pila toroide]
   (let [dato (get toroide posicion)]
     (if (or (nil? dato) (= dato \"))
       pila
       (modo-cadena (+ posicion 1) (conj pila (int dato)) toroide)
         ) ) )

;;funciones de calculo basicas
(defn sumar [valor1 valor2] (+ valor1 valor2))
(defn restar [valor1 valor2] (- valor1 valor2))
(defn multiplicar [valor1 valor2] (* valor1 valor2))
(defn dividir [valor1 valor2] (/ valor1 valor2))
(defn raiz [valor1 valor2] (% valor1 valor2))
(defn negado [valor] (if (zero? valor) 1 0))
(defn mayor [valor1 valor2] (if (> valor1 valor2) 1 0))
;;funciones de movimiento
(defn derecha [contador-programa] [(+ (first contador-programa) 1) (second contador-programa)])
(defn izquierda [contador-programa] [(- (first contador-programa) 1) (second contador-programa)])
(defn abajo [contador-programa] [(first contador-programa) (+ (second contador-programa) 1)])
(defn arriba [contador-programa] [(first contador-programa) (- (second contador-programa) 1)])
(defn random [contador-programa] (let [eleccion-random (rand 4)]
                   (case eleccion-random
                     0 (derecha contador-programa)
                     1 (izquierda contador-programa)
                     2 (arriba contador-programa)
                     3 (abajo contador-programa) )))
;;funciones condicionales
(defn if-horizontal [booleano contador-programa] (if (true? booleano) (izquierda contador-programa) (derecha contador-programa)))
(defn if-vertical [booleano contador-programa] (if (true? booleano) (arriba contador-programa) (abajo contador-programa)))