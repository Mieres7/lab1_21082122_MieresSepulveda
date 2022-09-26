#lang racket
(provide getPosX)
(provide getPosY)
(provide getPixel)
(provide newPosX)
(provide newPosY)
(provide n_componentes?)


;----------------------------------------------------------TDA - pixel---------------------------------------------------------;

; Implementacion del TDA pixel
; Representación: x(int) X y(int) X Bit(int) | Hex(str) | Red(int) X Green(int) X Blue(int) X Depth(int)


;----------------------------------------------------------CONSTRUCTORES-------------------------------------------------------;


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

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;


;----------------------------------------------------------MODIFICADORES-------------------------------------------------------;

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
                  (- ( - dimension (getPosX Pixel))1)))

;----------------------------------------------------------OTRAS OPERACIONES---------------------------------------------------;

;Nombre: n_componentes?
;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél(pixbit-d | píxhex-d | pixrgb-d)
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(lambda (pixel)
                        (cond ((null? pixel) 0)
                              (else (+ 1 (n_componentes?(cdr pixel)))))
                        ))