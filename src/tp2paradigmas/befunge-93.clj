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

(defn cambiar-direccion [valor]
  (case valor
    case \< ("izquierda")
    case \> ("derecha")
    case \v ("abajo")
    case \^ ("arriba") ))


;;devuelve la nueva lista sin el ultimo dato agregado (LIFO)
;;si la lista esta vacia devuelve 0
(defn desapilar [estado]
  (let [pila (:pila estado)] (if (nil? (peek pila)) 0 (pop pila)))
  )

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
(defn sumar [estado]
  (let [pila (:pila estado)
        valor1 (peek pila)
        valor2 (peek (pop pila))
        nueva-pila (conj (pop (pop pila)) (+ valor1 valor2))]
    (assoc estado :pila nueva-pila)))


(defn restar [estado]   (let [pila (:pila estado)
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

(defn negado [valor] (if (zero? valor) 1 0))

(defn mayor [estado] (let [pila (:pila estado)
                           valor1 (peek pila)
                           valor2 (peek (pop pila))]
                     (if (> valor1 valor2)
                       (assoc estado :pila (conj (pop (pop pila)) 1))
                       (assoc estado :pila (conj (pop (pop pila)) 0)))) )

;;funciones de movimiento ;;modificar para que reciba y devuelve el estado nuevo
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
(defn if-horizontal [pila contador-programa]  (if (= (peek pila) 0) (derecha contador-programa) (izquierda contador-programa))) ;;manejar desapilado del dato
(defn if-vertical [pila contador-programa] (if (= (peek pila) 0) (abajo contador-programa) (arriba contador-programa))) ;;manejar desapilado del dato
(defn dup [pila] (conj pila (peek pila)))
(defn intercambiar [pila] (let [valor1 (peek pila) valor2 (peek (desapilar pila))]
                                    (conj (conj (desapilar (desapilar pila)) valor1) valor2)  ))
(defn print-int [pila] (let [valor (peek pila)] (println (str valor " ")) (pop pila)))
(defn print-ascii [pila] (let [valor (peek pila)] (if (nil? valor)
                                                    nil
                                                  ((if (char? valor) (println (int valor)) (println (int(first(str valor)))))(desapilar pila)) )))

(defn descartar-valor [estado] (let [pila (:pila estado)] (assoc estado :pila (pop pila))))
(defn interpretar-dato [pila contador-programa]
  {:+ (sumar pila)
   :- (restar pila)
   :* (multiplicar pila)
   :/ (dividir pila)
   :% (modulo pila)
   :! (negado pila)
   :' (mayor pila)
   :? (random contador-programa)
   :_ (if-horizontal pila contador-programa)
   :| (if-vertical pila contador-programa)
   :$ (desapilar pila)
   :. (print-int pila)
   :# ()
   :g ()
   :p ()
   :& ()
   })

  (defn interpretar-mov [direccion contador-programa]
  (case direccion
    case "izquierda" (izquierda contador-programa)
    case "derecha" (derecha contador-programa)
    case "abajo" (abajo contador-programa)
    case "arriba" (arriba contador-programa)))


(defn recorrer-toroide [toroide pila direccion contador-programa]
  (let [dato (get toroide contador-programa)]
    (if (= (dato) \space)
      (recorrer-toroide toroide pila direccion (interpretar-mov direccion contador-programa))
      ((if (=(dato) \@) nil (interpretar-dato pila contador-programa)) ) )
    )
  )
