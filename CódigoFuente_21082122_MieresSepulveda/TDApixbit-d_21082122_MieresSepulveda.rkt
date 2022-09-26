#lang racket
(provide pixbit-d)
(provide getBit)
(provide getDepth_Bit)
(provide esBit?)
(provide numString)
(require "TDAPixel_21082122_MieresSepulveda.rkt")


;----------------------------------------------------------TDA - pixbit-d------------------------------------------------------;

; Implementacion del TDA pixbit-d
; Representación: x(int) X y(int) X Bit(int) X Depth(int)

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;Nombre: pixbit-d
;Descripción: Función que crea un pixel del tipo pixbit-d. 
;Dominio: x(int) X y(int) X Bit(int) X Depth(int)
;Recorrido: pixbit-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixbit-d(lambda(PosX PosY bit d)(list PosX PosY bit d)))
;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getBit
;Descripción: Función que determina el Bit de un pixel dek tipo bit
;Dominio: Pixel(list)
;Recorrido: Bit(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getBit(lambda(pixel)
                (third pixel)))


;Nombre: getDepth_Bit
;Descripción: Función que retorna la profundidad de un pixel del tipo bit.
;Dominio: Pixel
;Recorrido: Depth(int)
;Tipo de recursión: No aplica
;Estrategia: No aplica.
(define getDepth_Bit(lambda(pixel)
                      (if (esBit? pixel)
                          (fourth pixel)
                          "El pixel ingresado no es del tipo pixbit-d."
                          )))



;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Nombre: esBit?
;Descripción: Función que determina si un pixel es del tipo Bit.
;Dominio: Pixel(pixbit-d)
;Recorrido: Bolean.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define esBit?(lambda(pixel)
                (cond((= (n_componentes? pixel) 6) '#f)
                     ((= (third pixel) 1) '#t)
                     ((= (third pixel) 0) '#t)
                       )))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Nombre: numString
;Descripción: Función que transforma el bit de un pixel a string
;Dominio: Pixel(pixibt-d)
;Recorrido: string
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define numString(lambda(pixel)
                   (number->string(getBit pixel))))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

                        