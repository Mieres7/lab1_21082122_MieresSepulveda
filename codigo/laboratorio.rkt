#lang racket


;__________________________________________________________ TDA IMAGEN ________________________________________________________;


;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;


;Nombre: pixrgb-d.
;Descripción: Función que crea un pixel del tipo pixrgb-d.
;Dominio: Cordenada en X, cordenada en Y, colores R, G, B y la profundidad d.
;Recorrido: Pixel tipo RGB-d
;Tipo de recursión: No aplica
(define pixrgb-d(
        lambda(PosX PosY R G B d)(
               list PosX PosY R G B d)))

;Nombre: pixbit-d
;Descripción: Función que crea un pixel del tipo pixbit-d. 
;Dominio: Cordenada en X, cordenada en Y, bit 0 ó 1, y la profundidad d.
;Recorrido: Pixel tipo bit-d
;Tipo de recursión: No aplica
(define pixbit-d(
        lambda(PosX PosY bit d)(
               list PosX PosY bit d)))

;Nombre: pixhex-d
;Descripción: Función que crea un pixel del tipo pixhex-d.
;Dominio: Cordenada en X, cordenada en Y, color en notación hexadecimal, y la profundidad d.
;Recorrido: Pixel tipo hex-d
;Tipo de recursión: No aplica
(define pixhex-d(
        lambda(PosX PosY hex d)(
               list PosX PosY hex d)))


;Nombre: image.
;Descripción: Función constructura de imagenes a partir de una dimension dada (ancho y alto) y un conjunto de pixeles del tipo RGB-d, bit-d o hex-d.
;Dominio: Ancho, altura, conjunto de pixeles (pixrgb-d, pixbit-d, píxhex-d)
;Recorrido: Imagen.
;Tipo de recursión: No aplica.
(define image(  
        lambda(ancho altura . pixeles)
         (if (= (* ancho altura)(n_componentes? pixeles))
             (list ancho altura pixeles)
              "Favor ingresar cantidad correcta de pixeles, o en su defecto la dimensión correcta de la imagen.")))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;


;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixrgb-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define pixmap?(
                lambda(imagen)
                 (if (= (n_componentes?(getPixeles imagen))6)
                                 #t
                                 #f)))
                 
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixbit-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define bitmap?(
                lambda(imagen)
                 (cond((= (n_componentes?(first(third imagen))) 6) '#f)
                                  ((= (getBit imagen) 1) '#t)
                                  ((= (getBit imagen) 0) '#t)
                                   )))

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixhex-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define hexmap?(
                lambda(imagen)
                 (if(hexadecimal? (third(first(third imagen))))
                                 #t
                                 #f)))

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexadecimal?(
                     lambda(color)
                      (string? color)))


;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(
                       lambda (lista)
                        (cond ((null? lista) 0)
                                        (else (+ 1 (n_componentes?(cdr lista)))))))


;Nombre: compressed?
;Descripción: Función que determina si una imagen está comprimida o no.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define compressed?(
                     lambda(imagen)
                      (if (= (* (getWidth imagen) (getHeight imagen)) (n_componentes?(getPixeles imagen)))
                          #t
                          #f)))
                       


;----------------------------------------------------------SELECTORES----------------------------------------------------------;


;Nombre: getBit.
;Descripción: Función extrae el 3er elemento de un pixel, bajo el contexto de la funcion bitmap? esta obtiene el bit almacenado en el pixel
;Dominio: Imagen.
;Recorrido: Bit 0 o 1.
;Tipo de recursión: No aplica.
(define getBit(
               lambda(imagen)
                (third(first(third imagen)))))


;Nombre: getPosX.
;Descripción: Funcion que obtiene la posicion en X del pixel evaluado.
;Dominio: Pixel.
;Recorrido: Coordenada en X.
;Tipo de recursión: No aplica.
(define getPosX(
                lambda(pixel)
                 (car pixel)))


;Nombre: getPosY.
;Descripción: Funcion que obtiene la posicion en X del pixel evaluado.
;Dominio: Pixel.
;Recorrido: Coordenada en Y.
;Tipo de recursión: No aplica.
(define getPosY(
                lambda(pixel)
                 (car(cdr pixel))))


;Nombre: getPixeles.
;Descripción: Funcion que retorna los pixeles de una imagen.
;Dominio: Imagen.
;Recorrido: Pixeles de Imagen.
;Tipo de recursion: No aplica
(define getPixeles(lambda(imagen)(
                                  third imagen)))

(define getWidth(
                 lambda(imagen)
                  (car imagen)))

(define getHeight(
                  lambda(imagen)
                   (car(cdr imagen))))


;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;


;Nombre: flipH
;Descripción: Función que invierte un imagen horizontalmente.
;Dominio: Imagen.
;Recorrido: Imagen invertida horizontalmente.
;Tipo de recursión:

;(define flipH(
 ;             lambda(imagen)
  ;            (if (odd?(getWidth imagen))
                   ;primer caso, filas impar
                   
                   ;segundo caso, filas par
   ;                )))

;(define filaImpar(
 ;                 lambda(lista_p )))
;-------------------------------------------------------OTRAS OPERACIONES------------------------------------------------------;

;ARREGLAR ESTAS FUNCIONES-----------------------------
(define image2(lambda (ancho altura . pixeles)
                (list ancho altura (dimPixeles pixeles 0 ancho))
                ))

(define dimPixeles(lambda (lista_p acum max)
                    (if (null? lista_p)
                        null
                        (if (= (+ acum 1)max)
                            (list (car lista_p)(dimPixeles (cdr lista_p) 0 max) )
                            (cons (car lista_p) (dimPixeles (cdr lista_p) (+ acum 1) max)
                                  )))))
;ARREGLAR----------------------------------------------
                

;nuevo list ref
 (define pixel-ref(lambda(lista_pixeles referencia)(
                                                  list-ref lista_pixeles referencia)))

;info colores

;caso bit

(define getBitP(lambda (pixel)(
                third pixel)))   ;obtiene el valor del bit 0 o 1, la diferencia con getBit es que en el otro entra una imagen, aqui un pixel

;caso rgb

(define getRed(lambda(pixel)(
                             third pixel)))

(define getGreen(lambda(pixel)(
                             fourth pixel)))

(define getBlue(lambda(pixel)(
                             fifth pixel)))

;caso hexadecimal

(define getHex(lambda(pixel)(
                             third pixel)))



(define mitadTrunc(lambda(filas)
                    ( - (/ filas 2) 0.5)))

;(define impar (lambda (lista_p acum ref)(
                                         ;(if (null? lista_p)
                                             ;null
                                            ; (if (< (getPosY)(mitadTruc ref))
                                           ;      (appen [bitmap-d])))













         
                       


