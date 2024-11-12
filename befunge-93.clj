(def width 80)
(def height 25)

;; Funcion para crear una grilla 80x25 con espacios
(defn crear-toroide []
  (vec (repeat height (vec (repeat width \space)))))
