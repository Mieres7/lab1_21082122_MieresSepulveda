#lang racket


; TDA pixeles:
; Las siguientes funciones definen la estructura basica (basada en listas) de los tres tipos de pixeles requeridos, rgb, bit y hexadecimal.


; Función que crea un pixrgb-d, toma como dominio las coordenadas "x" e "y", seguido de los 3 parámetros correspondientes a "RGB" y la profundidad "d". (Las comillas son solo para distingir, no
; corresponden a strings)
; El recorrido corresponde a un pixel, el cual contiene todos los parámetros ingresados en el dominio.

(define pixrgb-d(
        lambda(PosX PosY R G B d)(
               list PosX PosY R G B d)))


; Función que crea un pixbit-d, toma como dominio las coordenadas "x" e "y", seguido de un "0" o "1" (según corresponda) y la profundidad "d". (Las comillas son solo para distingir, no
; corresponden a strings)
; El recorrido corresponde a un pixel, el cual contiene todos los parámetros ingresados en el dominio.

(define pixbit-d(
        lambda(PosX PosY bit d)(
               list PosX PosY bit d)))

; Función que crea un pixhex-d, toma como dominio las coordenadas "x" e "y", seguido de el string "000000" (correspondiente a la notacion de colores hex) y la profundidad "d". (Las comillas son solo para distingir, no
; corresponden a strings)
; El recorrido corresponde a un pixel, el cual contiene todos los parámetros ingresados en el dominio.

(define pixhex-d(
        lambda(PosX PosY hex d)(
               list PosX PosY hex d)))



;TDA image - Constructor

; Función constructora de imágenes, crea una lista a con los distintos tipos de 

(define image(  
        lambda(ancho altura . pixeles)
         (list ancho altura pixeles)))

;---------------------------------------------


                                              
                                             
                                             
                                          
                                             






;---------------------------------------------


;Función que determina el largo de una lista
;Dominio: lista.
;Recorrido: entero.
(define largo_lista(
                    lambda (lista)(
                                   cond ((null? lista) 0)
                                        (else (+ 1 (largo_lista(cdr lista)))))))

(define pixmap?(
                lambda(imagen)(
                              if (= (largo_lista(first(third imagen)))6)
                                 #t
                                 #f)))
                 

(define bitmap?(
                lambda(imagen)(
                              cond((= (largo_lista(first(third imagen))) 6) '#f)
                                  ((= (third(first(third imagen))) 1) '#t)
                                  ((= (third(first(third imagen))) 0) '#t)
                                   )))
  
(define hexmap?(
                lambda(imagen)(
                               if(string? (third(first(third imagen))))
                                 #t
                                 #f)))


                    
                                


                               



         
                       


