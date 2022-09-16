#lang racket
(require "TDAPixel.rkt")
(provide getBit)
(provide getRed)
(provide getGreen)
(provide getBlue)
(provide getHex)
(provide coloresHex)
(provide coloresRgb)
(provide hexadecimal?)
(provide RGBHex)
(provide hexValueQ)
(provide hexValueR)
(provide opuesto)

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

(define getRed(lambda(pixel)(
                             third pixel)))

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
                     (cons (getHex(getPixel pixeles)) (coloresHex(cdr pixeles)))
                 )))

(define coloresRgb(lambda(pixeles)
                    (if (null? pixeles)
                        null
                        (cons  (list (getRed(getPixel pixeles))
                                     (getGreen(getPixel pixeles))
                                     (getBlue(getPixel pixeles)))
                              (coloresRgb(cdr pixeles)))
                        )))

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

(define opuesto(lambda(RGB)
                 (-(- 256 RGB)1)
                 ))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;
