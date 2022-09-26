#lang racket
(provide pixrgb-d)
(provide getDepth_RGB)
(provide getR)
(provide getG)
(provide getB)
(provide getD)
(provide esRGB?)
(provide incChR)
(provide incChG)
(provide incChB)
(provide incChD)
(provide setR)
(provide setG)
(provide setB)
(provide setD)
(provide coloresRgb)
(provide getColorsRGB)
(provide RGBHex)
(provide opuesto)
(provide hexValueQ)
(provide hexValueR)
(require "TDAPixel_21082122_MieresSepulveda.rkt")



;----------------------------------------------------------TDA - pixrgb-d------------------------------------------------------;

; Implementacion del TDA pixrgb-d
; Representación: x(int) X y(int) X Red(int) X Green(int) X Blue(int) X Depth(int)

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;Nombre: pixrgb-d.
;Descripción: Función que crea un pixel del tipo pixrgb-d.
;Dominio: x(int) X y(int) X Red(int) X Green(int) X Blue(int) X Depth(int)
;Recorrido: pixrgb-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixrgb-d(lambda(PosX PosY R G B d)(list PosX PosY R G B d)))

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getR
;Descripción: Función que obtiene el valor almacenado en el canal R de un pixel pixrgb-d
;Dominio: Pixel(pixrgb-d)
;Recorrido: R(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define getR(lambda(pixel)(third pixel)))

;Nombre: getG
;Descripción: Función que obtiene el valor almacenado en el canal G de un pixel pixrgb-d
;Dominio: Pixel(pixrgb-d)
;Recorrido: R(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define getG(lambda(pixel)(fourth pixel)))

;Nombre: getB
;Descripción: Función que obtiene el valor almacenado en el canal B de un pixel pixrgb-d
;Dominio: Pixel(pixrgb-d)
;Recorrido: R(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define getB(lambda(pixel)(fifth pixel)))

;Nombre: getD
;Descripción: Función que obtiene el valor almacenado en el canal de la profundidad de un pixel pixrgb-d
;Dominio: Pixel(pixrgb-d)
;Recorrido: R(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define getD(lambda(pixel)(sixth pixel)))

;Nombre: getDepth_RGB
;Descripción: Función que retorna la profundidad de un pixel del tipo RGB.
;Dominio: Pixel
;Recorrido: Depth(int)
;Tipo de recursión: No aplica
;Estrategia: No aplica.          
(define getDepth_RGB(lambda(pixel)
                      (if (esRGB? pixel)
                          (sixth pixel)
                          "El pixel ingresado no es del tipo pixrgb-d."
                          )))

;Nombre: coloresRgb
;Descripción: Función que retorna una lista con los colores de todos los pixeles entregados.
;Dominio: Pixeles(list)
;Recorrido: Colores(list)
;Tipo de recursión: Recursión natural, puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define coloresRgb(lambda(pixeles)
                    (if (null? pixeles)
                        null
                        (cons  (list (getR(first pixeles))
                                     (getG(first pixeles))
                                     (getB(first pixeles)))
                              (coloresRgb(cdr pixeles)))
                        )))

;Nombre: getColoresRGB
;Descripción: Función que retorna la composicion completa (R G B) de un pixel del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Colores((Red(int) X Green(int) X Blue(int))
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getColorsRGB(lambda(pixel)
                      (list(getR pixel)(getG pixel)(getB pixel))))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Nombre: esRGB?
;Descripción: Función que determina si un pixel es del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Bolean.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define esRGB?(lambda(pixel)
                (if (= (n_componentes? pixel)6 )
                    '#t
                    '#f)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Nombre: incChR
;Descripción: Función que incrementa el valor del canal R en una unidad.
;Dominio: Value(int)
;Recorrido: Value(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define incChR(lambda(value)
                (if (= value 255)
                    0
                    (+ value 1))))

;Nombre: incChG
;Descripción: Función que incrementa el valor del canal G en una unidad.
;Dominio: Value(int)
;Recorrido: Value(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define incChG(lambda(value)
                (if (= value 255)
                    0
                    (+ value 1))))

;Nombre: incChB
;Descripción: Función que incrementa el valor del canal B en una unidad.
;Dominio: Value(int)
;Recorrido: Value(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define incChB(lambda(value)
                (if (= value 255)
                    0
                    (+ value 1))))

;Nombre: incChD
;Descripción: Función que incrementa el valor del canal de la profundidad en una unidad.
;Dominio: Value(int)
;Recorrido: Value(int)
;Recursión: No aplica.
;Estrategia: No aplica.
(define incChD(lambda(value)
               (+ value 1)))

;Nombre: setR
;Descripción: Función que modifica el canal R de un pixel pixrgb-d.
;Dominio: Pixel(pixrtgb-d) X Value(int)
;Recorrido: Pixel(pixrgb-d)
;Recursión: No aplica.
;Estrategia: No aplica.
(define setR(lambda(pixel value)
              (pixrgb-d (getPosX pixel)
                        (getPosY pixel)
                        value
                        (fourth pixel)
                        (fifth pixel)
                        (sixth pixel))
              ))

;Nombre: setG
;Descripción: Función que modifica el canal G de un pixel pixrgb-d.
;Dominio: Pixel(pixrtgb-d) X Value(int)
;Recorrido: Pixel(pixrgb-d)
;Recursión: No aplica.
;Estrategia: No aplica.
(define setG(lambda(pixel value)
              (pixrgb-d (getPosX pixel)
                        (getPosY pixel)
                        (third pixel)
                        value
                        (fifth pixel)
                        (sixth pixel))
              ))

;Nombre: setB
;Descripción: Función que modifica el canal B de un pixel pixrgb-d.
;Dominio: Pixel(pixrtgb-d) X Value(int)
;Recorrido: Pixel(pixrgb-d)
;Recursión: No aplica.
;Estrategia: No aplica.
(define setB(lambda(pixel value)
              (pixrgb-d (getPosX pixel)
                        (getPosY pixel)
                        (third pixel)
                        (fourth pixel)
                        value
                        (sixth pixel))
              ))

;Nombre: setD
;Descripción: Función que modifica el canal de la profundidad en un pixel pixrgb-d.
;Dominio: Pixel(pixrtgb-d) X Value(int)
;Recorrido: Pixel(pixrgb-d)
;Recursión: No aplica.
;Estrategia: No aplica.
(define setD(lambda(pixel value)
              (pixrgb-d (getPosX pixel)
                        (getPosY pixel)
                        (third pixel)
                        (fourth pixel)
                        (fifth pixel)
                        value)
              ))

;Nombre: RGBHex
;Descripción: Función que transforma los colores Reg, Green y Blue (RGB) a notación hexadecimal.
;Dominio: Red(int) X Green(int) X Blue(int)
;Recorrido: Hex(str)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define RGBHex(lambda (R G B)
                (string-append "#"
                               (hexValueQ R)(hexValueR R)
                               (hexValueQ G)(hexValueR G)
                               (hexValueQ B)(hexValueR B)
                               )))


;Nombre: opuesto
;Descripción: Función que calcula el valor simetricamente opuesto de un color en notación RGB. 
;Dominio: Red(int) | Green(int) | Blue(int)
;Recorrido: opuestoRed(int) | opuestoGreen(int) | opuestoBlue(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define opuesto(lambda(RGB)
                 (-(- 256 RGB)1)))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;Nombre: hexValueQ
;Descripción: Función que retorna un valor hexadecimal, dependiendo de la parte entera del resultado de dividir Red, Green o Blue
;             entre 16.
;Dominio: Red(int) | Green(int) | Blue(int)
;Recorrido: Hex(str)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define hexValueQ(lambda(colorRGB)
                   (case (quotient colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))


;Nombre: hexValueR
;Descripción: Función que retorna un valor hexadecimal, dependiendo del resto del resultado de dividir Red, Green o Blue
;             entre 16.
;Dominio: Red(int) | Green(int) | Blue(int)
;Recorrido: Hex(str)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define hexValueR(lambda(colorRGB)
                   (case (remainder colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))