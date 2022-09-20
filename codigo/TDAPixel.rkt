#lang racket
(provide pixrgb-d)
(provide pixbit-d)
(provide pixhex-d)
(provide getPosX)
(provide getPosY)
(provide getPixel)
(provide getDepth_Bit)
(provide getDepth_RGB)
(provide getDepth_Hex)
(provide esBit?)
(provide esRGB?)
(provide esHex?)
(provide newPosX)
(provide newPosY)
(provide n_componentes?)

;------------------------------------------------------------------------------------------------------------------------------;


;----------------------------------------------------------TDA - Pixel---------------------------------------------------------;

; Implementacion del TDA Pixel.
; Representación:
; - Tipo pixbit: (List x(int) X y(int) X Bit(int) X Depth(int))
; - Tipo RGB: (list x(int) X y(int) X Red(int) X Green(int) X Blue(int) X Depth(int))
; - Tipo Hex: (List x(int) X y(int) X Hex(str) X Depth(int))

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;Nombre: pixrgb-d.
;Descripción: Función que crea un pixel del tipo pixrgb-d.
;Dominio: x(int) X y(int) X Red(int) X Green(int) X Blue(int) X Depth(int)
;Recorrido: pixrgb-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixrgb-d(lambda(PosX PosY R G B d)(list PosX PosY R G B d)))

;Nombre: pixbit-d
;Descripción: Función que crea un pixel del tipo pixbit-d. 
;Dominio: x(int) X y(int) X Bit(int) X Depth(int)
;Recorrido: pixbit-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixbit-d(lambda(PosX PosY bit d)(list PosX PosY bit d)))

;Nombre: pixhex-d
;Descripción: Función que crea un pixel del tipo pixhex-d.
;Dominio: x(int) X y(int) X Hex(str) X Depth(int)
;Recorrido: pixhex-d
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixhex-d(lambda(PosX PosY hex d)(list PosX PosY hex d)))

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getPosX.
;Descripción: Funcion que obtiene la posicion en X del pixel evaluado.
;Dominio: Pixel(list)
;Recorrido: x(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getPosX(lambda(pixel)(car pixel)))


;Nombre: getPosY.
;Descripción: Funcion que obtiene la posicion en X del pixel evaluado.
;Dominio: Pixel(list)
;Recorrido: y(int)
;Tipo de recursión: No aplica.
;Estrategia: No alpica.
(define getPosY(lambda(pixel)(car(cdr pixel))))


;Nombre: getPixel
;Descripción: Función que retorna el primer pixel perteneciente a una lista de pixeles.
;Dominio: Pixeles(list)
;Recorrido: Pixel
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getPixel(lambda (pixeles)
                  (first pixeles)))

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

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Nombre: newPosY
;Descripción: Función que determina la nueva posicion de la coordenada Y de un pixel, bajo el contexto de las funciones flipV y flipH.
;Dominio: Pixel(pixbit-d | píxhex-d | pixrgb-d)
;Recorrido: y(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define newPosY(lambda(dimension Pixel)
                  (- ( - dimension (getPosY Pixel))1) )
 )

;Nombre: newPosX
;Descripción: Función que determina la nueva posicion de la coordenada X de un pixel, bajo el contexto de las funciones flipV y flipH.
;Dominio: Pixel(pixbit-d | píxhex-d | pixrgb-d)
;Recorrido: x(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define newPosX(lambda(dimension Pixel)
                  (- ( - dimension (getPosX Pixel))1) )
 )

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;Nombre: n_componentes?
;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél(pixbit-d | píxhex-d | pixrgb-d)
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(lambda (pixel)
                        (cond ((null? pixel) 0)
                              (else (+ 1 (n_componentes?(cdr pixel)))))
                        ))