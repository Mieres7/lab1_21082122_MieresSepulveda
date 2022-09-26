#lang racket
(provide pixhex-d)
(provide getDepth_Hex)
(provide esHex?)
(provide getHex)
(provide coloresHex)
(provide hexadecimal?)


;----------------------------------------------------------TDA - pixbit-d------------------------------------------------------;

; Implementacion del TDA pixbit-d
; Representación: x(int) X y(int) X Hex(str) X Depth(int)

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;Nombre: pixhex-d
;Descripción: Función que crea un pixel del tipo pixhex-d.
;Dominio: x(int) X y(int) X Hex(str) X Depth(int)
;Recorrido: pixhex-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixhex-d(lambda(PosX PosY hex d)(list PosX PosY hex d)))

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getHex
;Descripción: Función que obtiene el color de una pixel del tipo hexadecimal.
;Dominio: Pixel(pixhex-d)
;Recorrido: hex(str)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getHex(lambda(pixel)(third pixel)))

;Nombre: getDepth_Hex
;Descripción: Función que retorna la profundidad de un pixel del tipo hex.
;Dominio: Pixel
;Recorrido: Depth(int)
;Tipo de recursión: No aplica
;Estrategia: No aplica.
(define getDepth_Hex(lambda(pixel)
                      (if (esHex? pixel)
                          (fourth pixel)
                          "El pixel ingresado no es del tipo pixhex-d."
                          )))

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

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Nombre: esHex?
;Descripción: Función que determina si un pixel es del tipo Hex.
;Dominio: Pixel(pixhex-d)
;Recorrido: Bolean.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define esHex?(lambda(pixel)
                (if(string?(third pixel))
                                 #t
                                 #f)))

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color(str).
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define hexadecimal?(lambda(color)
                      (string? color)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;


;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;
