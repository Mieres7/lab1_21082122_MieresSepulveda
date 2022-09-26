#lang racket
(provide bitHistogram)
(provide rgbHistogram)
(provide hexHistogram)
(require "TDApixbit-d_21082122_MieresSepulveda.rkt")
(require "TDApixrgb-d_21082122_MieresSepulveda.rkt")
(require "TDApixhex-d_21082122_MieresSepulveda.rkt")
(require "TDAPixel_21082122_MieresSepulveda.rkt")
(require "TDAPixeles_21082122_MieresSepulveda.rkt")

;----------------------------------------------------------TDA - Histogram-------------------------------------------------------;

; Implementación del TDA Image
; Representación: (List(Color(int) | Color(str) | Color(Red(int) | Green(int) | Blue(int)) X Cantidad(int)))


;----------------------------------------------------------COSNTRUCTORES---------------------------------------------------------;

;Nombre: bitHistogram
;Descripción: Función que determina el histograma de una función del tipo bitmap.
;Dominio: Pixeles(list) X Ancho(int) X Alto(int)
;Recorrido: Color(int) X Cantidad(int)
;Tipo de recursión: Recursión de Cola. Es necesaria, puesto que la solución es necesaria que se construya una vez recorrida la
;                   la lista de pixeles.
;Estrategia: No aplica
(define bitHistogram(lambda(pixeles ancho alto)
                         (define bitHistogramInt(lambda(pixeles ancho alto sumaBit)
                                               (if (null? pixeles)
                                                   (list(list 0 sumaBit)(list 1 (-(* ancho alto)sumaBit)))
                                                   (cond ((=(getBit(getPixel pixeles))0) (bitHistogramInt (cdr pixeles) ancho alto (+ sumaBit 1)))
                                                         (else (bitHistogramInt (cdr pixeles) ancho alto sumaBit)))
                                                   )
                                               ))
                  (bitHistogramInt pixeles ancho alto 0)
                         ))

;Nombre: hexHistogram
;Descripción: Función que determina el histograma de una función del tipo hexmap.
;Dominio: Colores(list) X PrimerColor(str).
;Recorrido: Color(str) X Cantidad(int)
;Tipo de recursión: Recursión natural. Es necesaria, puesto que la lista de colores debe reducirse en cada llamado recursivo.
;Estrategia: No aplica.
(define hexHistogram(lambda(colores primerColor)
                      (if (null? colores)
                          null
                          (cons(list(car colores)
                               (n_pixeles?(filtro-px(lambda (color)
                                                      (string=? color primerColor))
                                                    colores)))
                               (hexHistogram (remove* (list primerColor) colores)(if (null?(remove* (list primerColor) colores))
                                                                                            null
                                                                                            (car(remove* (list primerColor) colores))
                                                                                      )) 
                               ))))


;Nombre: rgbHistogram
;Descripción: Función que determina el histograma de una función del tipo pixmap.
;Dominio: Colores(list) X PrimerColor(str).
;Recorrido: (Red(int) X Green(int) X Blue(int)) X Cantidad(int)
;Tipo de recursión: Recursión natural. Es necesaria, puesto que la lista de colores debe reducirse en cada llamado recursivo.
;Estrategia: Descomposición.
(define rgbHistogram(lambda(colores primerColor)
                      (if (null? colores)
                          null
                          (cons(list(car colores)
                               (n_pixeles?(filtro-px(lambda (color)
                                                      (equal? color primerColor))
                                                    colores)))
                               (rgbHistogram (remove* (list primerColor) colores)(if (null?(remove* (list primerColor) colores))
                                                                                            null
                                                                                            (car(remove* (list primerColor) colores))
                                                                                      )) 
                               ))))



;----------------------------------------------------------SELECTORES------------------------------------------------------------;

;----------------------------------------------------------PERTENENCIA-----------------------------------------------------------;

;----------------------------------------------------------MODIFICADORES---------------------------------------------------------;

;----------------------------------------------------------OTRAS OPERACIONES-------------------------------------------------------;


