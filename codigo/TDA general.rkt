#lang racket

;TDA - image

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

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


;----------------------------------------------------------SELECTORES-----------------------------------------------------------;

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

;----------------------------------------------------------PERTENENCIA----------------------------------------------------------;

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixrgb-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define pixmap?(
                lambda(imagen)
                 (if (= (n_componentes?(getPixel(getPixeles imagen)))6)
                                 #t
                                 #f)))
                 
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixbit-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define bitmap?(
                lambda(imagen)
                 (cond((= (n_componentes?(getPixel(getPixeles imagen))) 6) '#f)
                                  ((= (getBit(getPixel(getPixeles imagen))) 1) '#t)
                                  ((= (getBit(getPixel(getPixeles imagen))) 0) '#t)
                                   )))

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixhex-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define hexmap?(
                lambda(imagen)
                 (if(hexadecimal? (getPixel(getPixeles imagen)))
                                 #t
                                 #f)))

;Nombre: compressed?
;Descripción: Función que determina si una imagen está comprimida o no.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define compressed?(
                     lambda(imagen)
                      (if (= (* (getWidth imagen) (getHeight imagen)) (n_componentes?(getPixel(getPixeles imagen))))
                          #f
                          #t)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;


;TDA - pixel

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

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

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

;nuevo list ref
(define pixel-ref(lambda(lista_pixeles referencia)(
                                                  list-ref lista_pixeles referencia)
                   ))

;retorna el primer pixel de una lista de pixeles sacadas de una imagen.
(define getPixel(lambda (lista_pixeles)
                  (first lista_pixeles)))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(
                       lambda (pixel)
                        (cond ((null? pixel) 0)
                                        (else (+ 1 (n_componentes?(cdr pixel)))))))

;TDA - color

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getBit.
;Descripción: Función extrae el 3er elemento de un pixel, bajo el contexto de la funcion bitmap? esta obtiene el bit almacenado en el pixel
;Dominio: Imagen.
;Recorrido: Bit 0 o 1.
;Tipo de recursión: No aplica.
(define getBit(
               lambda(pixel)
                (third pixel)))

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

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexadecimal?(
                     lambda(color)
                      (string? color)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;

(define mitadTrunc(lambda(filas)
                    ( - (/ filas 2) 0.5)))

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;

;(define flipH (
 ;              lambda (imagen)))

(define invierte (lambda (lista_p ancho)
                   (cond (< (getPosY(getPixel lista_p))(mitad ancho) "hacer algo")
                         (> (getPosY(getPixel lista_p))(mitad ancho) "hacer algo")
                         (else "hacer algo mas"))
                   ))
                   
  








(define mitad(lambda(ancho)
               (if (odd? ancho)
                   (-(/ ancho 2)0.5)
                   (/ ancho 2)
                   )))

                
(define my-map(lambda (func lista)
                (map func lista)))

(define esBit?(lambda(pixel)
                (cond((= (n_componentes? pixel) 6) '#f)
                     ((= (getBit pixel) 1) '#t)
                     ((= (getBit pixel) 0) '#t)
                       )))

(define esRGB?(lambda(pixel)
                (if (= (n_componentes? pixel)6 )
                    '#t
                    '#f)))

(define esHex?(lambda(pixel)
                (if(hexadecimal?(getHex pixel))
                                 #t
                                 #f)))
                                                   
(define getDepth_Bit(lambda(pixel)
                      (if (esBit? pixel)
                          (fourth pixel)
                          "poner condicion."
                          )
                      ))

"mensaje de actualización, fallo en el commit anterior"
                 
(define getDepth_RGB(lambda(pixel)
                      (if (esRGB? pixel)
                          (sixth pixel)
                          "poner condicion."
                          )
                      ))

(define getDepth_Hex(lambda(pixel)
                      (if (esRGB? pixel)
                          (fourth pixel)
                          "poner condicion."
                          )
                      ))


(define eliminaPixelI (lambda (lista_pixeles)
                         (cdr lista_pixeles)))

(define eliminaPixelF (lambda (lista_pixeles)
                         (reverse(cdr(reverse lista_pixeles)))
                         ))

;elimina el primero y el ultimo de una lista
(define delIniFin (lambda (lista_pixeles)
                    (eliminaPixelI(eliminaPixelF lista_pixeles))
                   ))
 
;-- XD --
 (define esImpar2 (lambda(lista_pixeles filas acum)
                   (if (null? lista_pixeles)
                       null
                       (if(< (getPosY(getPixel lista_pixeles)) (mitadTrunc filas))
                           (cond ((esBit?(getPixel lista_pixeles)) (list (pixbit-d  (getPosX(pixel-ref lista_pixeles(- (+ acum filas) 1)))                     ;este es el else del primer if
                                                                                    (getPosY(pixel-ref lista_pixeles(- (+ acum filas) 1)))
                                                                                    (getBit(getPixel lista_pixeles))
                                                                                    (getDepth_Bit(getPixel lista_pixeles))                                                                                                            ;llamado recursivo va aqui
                                                                                    )
                                                                                    (esImpar (delIniFin lista_pixeles) (- filas 1) (+ acum 1)))
                                                                   )
                                 ;((esRGB?(getPixel lista_pixeles) (append [pixrgb-d (getPosX(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                  ;                                                  (getPosY(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                   ;                                                 (getRed(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                    ;                                                (getGreen(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                     ;                                               (getBlue(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                      ;                                              (getDepth_RGB(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                       ;                                             ]
                                                                          ;llamado recursivo 
                                                 ;   ))
                                  ;((esHex?(getPixel lista_pixeles) (append [pixhex-d (getPosX(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                   ;                                                  (getPosY(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                    ;                                                 (getHex(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                     ;                                                (getDepth_Hex(pixel-ref(getPixel(lista_pixeles))(+ acum filas)))
                                      ;                                               ]
                                                                           ;llamado recursivo
                                                  ;   ))
                                                                                     
                                                                                     
                                                                                    
                                 );))
                           "Hola";else del segundo if.
                           ))))



 (define esImpar (lambda(lista_pixeles filas acum columnas)
                   (if (null? lista_pixeles)
                       null
                       (if(<= (getPosY(getPixel lista_pixeles)) (mitadTrunc columnas))
                           (cond ((esBit?(getPixel lista_pixeles)) (list(pixbit-d   (getPosX(pixel-ref lista_pixeles(- columnas 1)))                     ;este es el else del primer if
                                                                                    (getPosY(pixel-ref lista_pixeles(- columnas 1)))
                                                                                    (getBit(getPixel lista_pixeles))
                                                                                    (getDepth_Bit(getPixel lista_pixeles))                                     ;llamado recursivo va aqui
                                                                                    )
                                                                          (pixbit-d (getPosX(getPixel lista_pixeles))                                          ;este es el else del primer if
                                                                                    (getPosY(getPixel lista_pixeles))
                                                                                    (getBit(pixel-ref lista_pixeles(- columnas 1)))
                                                                                    (getDepth_Bit(pixel-ref lista_pixeles(- columnas 1)))                ;llamado recursivo va aqui
                                                                                    )
                                                                          
                                                                                    (esImpar (delIniFin lista_pixeles) (- filas 2) (+ acum 2) columnas))
                                                                   )
                                                                                  
                                 )
                           (append (getPixel lista_pixeles))
                           ))))







