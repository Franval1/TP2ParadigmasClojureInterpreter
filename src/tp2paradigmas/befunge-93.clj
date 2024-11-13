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
