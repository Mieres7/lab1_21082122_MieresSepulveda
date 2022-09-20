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
(provide hexValueQ)
(provide hexValueR)
(require "TDAPixel_21082122_MieresSepulveda.rkt")
;------------------------------------------------------------------------------------------------------------------------------;


;----------------------------------------------------------TDA - Color---------------------------------------------------------;

; Implementación del TDA Color.
; Representación:
; - Bit: Color(int)                        -> 0 o 1
; - RGB: Red(int) X Green(int) X Blue(int) -> ex: 255 255 255
; - Hex: Color(str)                        -> ex: "#FFFFFF"

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getBit
;Descripción: Función que determina el Bit de un pixel dek tipo bit
;Dominio: Pixel(list)
;Recorrido: Bit(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getBit(lambda(pixel)
                (third pixel)))

;Nombre: getRed
;Descripción: Función que obtiene el color rojo de un pixel del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Red(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getRed(lambda(pixel)(third pixel)))

;Nombre: getGreen
;Descripción: Función que obtiene el color verde de un pixel del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Green(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getGreen(lambda(pixel)(fourth pixel)))


;Nombre: getBlue
;Descripción: Función que obtiene el color azul de un pixel del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Blue(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getBlue(lambda(pixel)(fifth pixel)))


;Nombre: getHex
;Descripción: Función que obtiene el color de una pixel del tipo hexadecimal.
;Dominio: Pixel(pixhex-d)
;Recorrido: hex(str)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getHex(lambda(pixel)(third pixel)))


;Nombre: coloresHex
;Descripción: Función que retorna una lista con los colores de todos los pixeles entregados.
;Dominio: Pixeles(list)
;Recorrido: Colores(list)
;Tipo de recursión: Recursión natural, puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define coloresHex(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (getHex(first pixeles)) (coloresHex(cdr pixeles)))
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
                        (cons  (list (getRed(first pixeles))
                                     (getGreen(first pixeles))
                                     (getBlue(first pixeles)))
                              (coloresRgb(cdr pixeles)))
                        )))

;Nombre: getColoresRGB
;Descripción: Función que retorna la composicion completa (R G B) de un pixel del tipo RGB.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Colores((Red(int) X Green(int) X Blue(int))
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getColorsRGB(lambda(pixel)
                      (list(getRed pixel)(getGreen pixel)(getBlue pixel))))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color(str).
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define hexadecimal?(lambda(color)
                      (string? color)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

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

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;

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