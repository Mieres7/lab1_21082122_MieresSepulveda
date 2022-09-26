#lang racket
(provide getWidth)
(provide getHeight)
(provide nuevaDim)
(provide filtroCrop)
(provide histogramCase)
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "TDApixbit-d_21082122_MieresSepulveda.rkt")
(require "TDApixrgb-d_21082122_MieresSepulveda.rkt")
(require "TDApixhex-d_21082122_MieresSepulveda.rkt")
(require "TDAPixel_21082122_MieresSepulveda.rkt")


;------------------------------------------------------------------------------------------------------------------------------;



;----------------------------------------------------------TDA - Image---------------------------------------------------------;

; Implementación del TDA Image
; Representación:
; - Image: Ancho(int+) X Alto(int+) X Pixeles(list)
; - Image Compressed: Ancho(int) X Alto(int) X Depths(list X int) X Color(int) | Color(str) | Color(Red(int) | Green(int) | Blue(int)


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

                   
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixbit-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define bitmap?(
                lambda(image)
                 (cond ((string?(getBit(getPixel(getPixeles image)))) '#f)
                       ((= (getBit(getPixel(getPixeles image))) 1) '#t)
                       ((= (getBit(getPixel(getPixeles image))) 0) '#t)
                       ((= (n_componentes?(getPixel(getPixeles image))) 6) '#f)
                      
                      )
                 ))


;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixrgb-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define pixmap?(
                lambda(image)
                 (if (= (n_componentes?(getPixel(getPixeles image)))6)
                                 #t
                                 #f)))

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixhex-d.
;Dominio: Imagen.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
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





                                     



                


                     










  
  
  







