#lang racket
(provide pixbit-d)
(provide getBit)
(provide getPosX)
(provide getPosY)
(provide getPixel)
(provide getDepth_Bit)
(provide esBit?)
(provide newPosX)
(provide newPosY)
(provide n_componentes?)
(provide numString)


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


;Nombre: numString
;Descripción: Función que transforma el bit de un pixel a string
;Dominio: Pixel(pixibt-d)
;Recorrido: string
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define numString(lambda(pixel)
                   (number->string(getBit pixel))))

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