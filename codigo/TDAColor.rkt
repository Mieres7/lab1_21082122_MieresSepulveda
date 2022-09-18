#lang racket
(provide getBit)
(provide getRed)
(provide getGreen)
(provide getBlue)
(provide getHex)
(provide coloresHex)
(provide coloresRgb)
(provide getColorsRGB)
(provide hexadecimal?)
(provide RGBHex)
(provide opuesto)
(provide invertColorRGB)
(provide invertColorBit)
(provide hexValueQ)
(provide hexValueR)
(require "TDAPixel.rkt")

;TDA - color

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getBit.
;Descripción: Función extrae el 3er elemento de un pixel, bajo el contexto de la funcion bitmap? esta obtiene el bit almacenado en el pixel
;Dominio: Imagen.
;Recorrido: Bit 0 o 1.
;Tipo de recursión: No aplica.
(define getBit(lambda(pixel)
                (third pixel)))

;caso rgb

(define getRed(lambda(pixel)(third pixel)))

(define getGreen(lambda(pixel)(
                             fourth pixel)))

(define getBlue(lambda(pixel)(
                             fifth pixel)))

;caso hexadecimal

(define getHex(lambda(pixel)
                (third pixel)))



;las 2 siguientes dejan los colores en solo una lista
(define coloresHex(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (getHex(first pixeles)) (coloresHex(cdr pixeles)))
                 )))

(define coloresRgb(lambda(pixeles)
                    (if (null? pixeles)
                        null
                        (cons  (list (getRed(first pixeles))
                                     (getGreen(first pixeles))
                                     (getBlue(first pixeles)))
                              (coloresRgb(cdr pixeles)))
                        )))

(define getColorsRGB(lambda(pixel)
                      (list(getRed pixel)(getGreen pixel)(getBlue pixel))))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexadecimal?(lambda(color)
                      (string? color)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

(define RGBHex(lambda (R G B)
                (string-append "#"
                               (hexValueQ R)(hexValueR R)
                               (hexValueQ G)(hexValueR G)
                               (hexValueQ B)(hexValueR B)
                               )))



(define opuesto(lambda(RGB)
                 (-(- 256 RGB)1)
                 ))

(define invertColorBit(lambda(pixel)
                        (pixbit-d (getPosX pixel)
                                  (getPosY pixel)
                                  (if (=(getBit pixel)1)
                                      0
                                      1)
                                  (getDepth_Bit pixel))
                        ))

(define invertColorRGB(lambda(pixel)
                        (pixrgb-d (getPosX pixel)
                                  (getPosY pixel)
                                  (opuesto(getRed pixel))
                                  (opuesto(getGreen pixel))
                                  (opuesto(getBlue pixel))
                                  (getDepth_RGB pixel))
                        ))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;
(define hexValueQ(lambda(colorRGB)
                   (case (quotient colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))

(define hexValueR(lambda(colorRGB)
                   (case (remainder colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))