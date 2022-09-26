#lang racket
(provide getWidth)
(provide getHeight)
(provide nuevaDim)
(provide filtroCrop)
(provide histogramCase)
(provide bitHistogram)
(provide rgbHistogram)
(provide hexHistogram)
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "TDApixbit-d_21082122_MieresSepulveda.rkt")
(require "TDApixrgb-d_21082122_MieresSepulveda.rkt")
(require "TDApixhex-d_21082122_MieresSepulveda.rkt")

;------------------------------------------------------------------------------------------------------------------------------;



;----------------------------------------------------------TDA - Image---------------------------------------------------------;

; Implementación del TDA Image
; Representación:
; - Image: Ancho(int+) X Alto(int+) X Pixeles(list)
; - Image Compressed: Ancho(int) X Alto(int) X Depths(list X int) X Color(int) | Color(str) | Color(Red(int) | Green(int) | Blue(int)
; - Histogram: (List(Color(int) | Color(str) | Color(Red(int) | Green(int) | Blue(int)) X Cantidad(int)))


;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;----------------------------------------------------------SELECTORES-----------------------------------------------------------;

;Nombre: getWidth
;Descripción: Función que retorna el ancho de una imagen
;Dominio: image
;Recorrido: Ancho(int+)
;Tipo de recursión: No aplica
;Estrategia: No aplica
(define getWidth(lambda(image)
                  (car image)))


;Nombre: getHeight
;Descripción: Función que retorna el alto de una imagen
;Dominio: Image.
;Recorrido: Alto(int+)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define getHeight(lambda(image)
                   (car(cdr image))))

;----------------------------------------------------------PERTENENCIA----------------------------------------------------------;

;Nombre: bitmap?
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixbit-d.
;Dominio: Image.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define bitmap?(
                lambda(image)
                 (cond((= (getBit(getPixel(getPixeles image))) 1) '#t)
                      ((= (getBit(getPixel(getPixeles image))) 0) '#t)
                      ((= (n_componentes?(getPixel(getPixeles image))) 6) '#f)
                      )
                 ))

;Nombre: pixmap?
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixrgb-d.
;Dominio: Image.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define pixmap?(
                lambda(image)
                 (if (= (n_componentes?(getPixel(getPixeles image)))6)
                                 #t
                                 #f)))
                 

;Nombre: hexmap?
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixhex-d.
;Dominio: Image.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define hexmap?(lambda(image)
                 (if(hexadecimal?(getHex(getPixel(getPixeles image))))
                                 #t
                                 #f)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Nombre: nuevaDim
;Descripción: Función que determina la nueva dimesión que tendrá una imagen tras sometarla a la función crop.
;Dominio: d1(int) X d2(int).
;Recorrido: int+.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define nuevaDim(lambda(d1 d2)
                  (+(- d2 d1)1)
                  ))
 
;Nombre: filtroCrop
;Descripción: Función que aplica un filtro a los pixeles de una imagen, en este caso la condición que debe cumplir cada pixel,
;             es pertenecer al área especificada en la entrada.
;Dominio: image X pixel X x1(int) X y1(int) X x2(int) X y2(int).
;Recorrido: Pixeles(list)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define filtroCrop(lambda(image pixel x1 y1 x2 y2)
                    (filtro-px(lambda(pixel)(and
                               (>= (getPosX pixel) x1)(<= (getPosX pixel) x2)
                               (>= (getPosY pixel) y1)(<= (getPosY pixel) y2)
                               ))(getPixeles image)
  )
                    ))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

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


;Nombre: histogramCase
;Descripción: Función que retorna 0,1 o 2, según el tipo de imagen ingresada.
;Dominio: Image.
;Recorrido: caso(int)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define histogramCase(lambda(image)
                       (cond ((hexmap? image) 0)
                             ((bitmap? image) 1)
                             ((pixmap? image) 2)
                             )
                       ))






                                     



                


                     










  
  
  







