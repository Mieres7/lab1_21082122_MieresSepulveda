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
                 (cond((= (getBit(getPixel(getPixeles imagen))) 1) '#t)
                      ((= (getBit(getPixel(getPixeles imagen))) 0) '#t)
                      ((= (n_componentes?(getPixel(getPixeles imagen))) 6) '#f)
                      )
                 ))

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixhex-d.
;Dominio: Imagen.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexmap?(
                lambda(imagen)
                 (if(hexadecimal?(getHex(getPixel(getPixeles imagen))))
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

;Documentacion
(define flipH (lambda (imagen)
                (cond ((bitmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirBitH(getWidth imagen)(getPixeles imagen))))
                      ((pixmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirRGBH(getWidth imagen)(getPixeles imagen))))
                      ((hexmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirHexH(getWidth imagen)(getPixeles imagen))))

                 )
               )
)

(define flipV (lambda (imagen)
                (cond ((bitmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirBitV(getHeight imagen)(getPixeles imagen))))
                      ((pixmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirRGBV(getHeight imagen)(getPixeles imagen))))
                      ((hexmap? imagen) (list (getWidth imagen)(getHeight imagen)(invertirHexV(getHeight imagen)(getPixeles imagen))))

                 )
               )
)

(define imgRGB->imgHex(lambda (imagen)
                        (list (getWidth imagen)(getHeight imagen)(convert(getPixeles imagen)))
                        ))

(define crop(lambda(imagen x1 y1 x2 y2)
              (list (nuevaDim x1 x2)(nuevaDim y1 y2)(filtroCrop imagen (getPixel(getPixeles imagen)) x1 y1 x2 y2)
                     )
              ))

(define nuevaDim(lambda(d1 d2)
                  (+(- d2 d1)1)
                  ))

(define filtroCrop(lambda(imagen pixel x1 y1 x2 y2)
                    (filtro-px(lambda(pixel)(and
                               (>= (getPosX pixel) x1)(<= (getPosX pixel) x2)
                               (>= (getPosY pixel) y1)(<= (getPosY pixel) y2)
                               ))(getPixeles imagen)
  )
                    ))


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
;Dominio: Cordenada en X, cordenada en Y, color en notación hexadecimal ("#" inlcuido al inicio), y la profundidad d.
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



;retorna el primer pixel de una lista de pixeles sacadas de una imagen.
(define getPixel(lambda (pixeles)
                  (first pixeles)))

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

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

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Documentacion
(define invertirBitH(lambda(ancho pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixbit-d (getPosX(getPixel pixeles))
                                      (newPosY ancho(getPixel pixeles))
                                      (getBit(getPixel pixeles))
                                      (getDepth_Bit(getPixel pixeles)))
                            (invertirBitH ancho (cdr pixeles))
                                  ))))

;Documentacion
(define invertirRGBH(lambda(ancho pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixrgb-d (getPosX(getPixel pixeles))
                                      (newPosY ancho(getPixel pixeles))
                                      (getRed(getPixel pixeles))
                                      (getGreen(getPixel pixeles))
                                      (getBlue(getPixel pixeles))
                                      (getDepth_RGB(getPixel pixeles)))
                            (invertirRGBH ancho (cdr pixeles))
                                  ))))

;Documentacion
(define invertirHexH(lambda(ancho pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixhex-d (getPosX(getPixel pixeles))
                                      (newPosY ancho(getPixel pixeles))
                                      (getHex(getPixel pixeles))
                                      (getDepth_Hex(getPixel pixeles)))
                            (invertirHexH ancho (cdr pixeles))
                                  ))))

;Documentacion
(define invertirBitV(lambda(altura pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixbit-d (newPosX altura (getPixel pixeles))
                                      (getPosY(getPixel pixeles))
                                      (getBit(getPixel pixeles))
                                      (getDepth_Bit(getPixel pixeles)))
                            (invertirBitV altura (cdr pixeles))
                                  ))))

;Documentacion
(define invertirRGBV(lambda(altura pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixrgb-d (newPosX altura (getPixel pixeles))
                                      (getPosY(getPixel pixeles))
                                      (getRed(getPixel pixeles))
                                      (getGreen(getPixel pixeles))
                                      (getBlue(getPixel pixeles))
                                      (getDepth_RGB(getPixel pixeles)))
                            (invertirRGBV altura (cdr pixeles))
                                  ))))

;Documentacion
(define invertirHexV(lambda(altura pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixhex-d (newPosX altura (getPixel pixeles))
                                      (getPosY(getPixel pixeles))
                                      (getHex(getPixel pixeles))
                                      (getDepth_Hex(getPixel pixeles)))
                            (invertirHexV altura (cdr pixeles))
                                  ))))


;Determina la nueva posiscion en Y que tendrá el pixel a evaluar
(define newPosY(lambda(dimension Pixel)
                  (- ( - dimension (getPosY Pixel))1) )
 )

(define newPosX(lambda(dimension Pixel)
                  (- ( - dimension (getPosX Pixel))1) )
 )

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(lambda (pixel)
                        (cond ((null? pixel) 0)
                              (else (+ 1 (n_componentes?(cdr pixel)))))
                        ))

(define n_pixeles?(lambda (pixeles)
                        (cond ((null? pixeles) 0)
                              (else (+ 1 (n_pixeles?(cdr pixeles)))))
                    ))

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

(define getHex(lambda(pixel)
                (third pixel)))

(define getDepth_Bit(lambda(pixel)
                      (if (esBit? pixel)
                          (fourth pixel)
                          "poner condicion."
                          )
                      ))

                 
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

;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexadecimal?(
                     lambda(color)
                      (string? color)))

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

(define RGBHex(lambda (R G B)
                (string-append "#"
                               (hexValueQ R)(hexValueR R)
                               (hexValueQ G)(hexValueR G)
                               (hexValueQ B)(hexValueR B)
                               )))

(define hexValueQ(lambda(colorRGB)
                   (case (quotient colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))

(define hexValueR(lambda(colorRGB)
                   (case (remainder colorRGB 16)
                     [(0)"0"][(1)"1"][(2)"2"][(3)"3"][(4)"4"][(5)"5"][(6)"6"][(7)"7"]
                     [(8)"8"][(9)"9"][(10)"A"][(11)"B"][(12)"C"][(13)"D"][(14)"E"][(15)"F"])
                   ))

;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;

;Las 4 siguientes no las he usado pero podria
(define my-map(lambda (func lista)
                (map func lista)))

(define eliminaPixelI (lambda (lista_pixeles)
                         (cdr lista_pixeles)))

(define eliminaPixelF (lambda (lista_pixeles)
                         (reverse(cdr(reverse lista_pixeles)))
                         ))

;elimina el primero y el ultimo de una lista
(define delIniFin (lambda (lista_pixeles)
                    (eliminaPixelI(eliminaPixelF lista_pixeles))
                   ))

; TDA pixeles por definir
(define convert(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (pixhex-d (getPosX(getPixel pixeles))
                                     (getPosY(getPixel pixeles))
                                     (RGBHex (getRed(getPixel pixeles))(getGreen(getPixel pixeles))(getBlue(getPixel pixeles)))
                                     (getDepth_RGB(getPixel pixeles)))
                           (convert(cdr pixeles))))))

;--------------------------------------------------------OPERACIONES TDA PO----------------------------------------------------;



(define histogram(lambda(imagen)
                   (case (histogramCase imagen)
                     [(0)(hexHistogram (getPixeles imagen)(getHex(getPixel(getPixeles imagen))))]
                     [(1)(bitHistogramEnv (getPixeles imagen)(getWidth imagen)(getHeight imagen))]
                   )
                   ))


(define bitHistogramEnv(lambda(pixeles ancho alto)
                         (define bitHistogram(lambda(pixeles ancho alto sumaBit)
                                               (if (null? pixeles)
                                                   (list(list 0 sumaBit)(list 1 (-(* ancho alto)sumaBit)))
                                                   (cond ((=(getBit(getPixel pixeles))0) (bitHistogram (cdr pixeles) ancho alto (+ sumaBit 1)))
                                                         (else (bitHistogram (cdr pixeles) ancho alto sumaBit)))
                                                   )
                                               ))
                  (bitHistogram pixeles ancho alto 0)
                         ))


(define filtro-px(lambda(filtro pixeles)
                   (filter filtro pixeles)))

(define hexHistogram(lambda(pixeles colorPrimerPixel)
                      (if (null? pixeles)
                          null
                          (list (getHex(getPixel pixeles))
                                (n_pixeles?(filtro-px(lambda(pixel)
                                                       (string=? (getHex pixel) colorPrimerPixel))
                                                     pixeles))
                                
                                )
                          ) 
                      ))
                          

                        

                          
(define histogramCase(lambda(imagen)
                       (cond ((hexmap? imagen) 0)
                             ((bitmap? imagen) 1)
                             ((pixmap? imagen) 2)
                             )
                       ))

               
                   
                              
(define img1(image 2 2
                   (pixrgb-d 0 0 255 0 0 10)
                   (pixrgb-d 0 1 0 255 0 20)
                   (pixrgb-d 1 0 0 0 255 10)
                   (pixrgb-d 1 1 255 255 255  1)
                   ))

(define img2(image 2 2
                   (pixhex-d 0 0 "#FF0000" 10)
                   (pixhex-d 0 1 "#00FF00" 20)
                   (pixhex-d 1 0 "#00FF00" 30)
                   (pixhex-d 1 1 "#FFFFFF" 40)
                    ))

(define img3(image 2 2
                   (pixbit-d 0 0 1 10)
                   (pixbit-d 0 1 1 20)
                   (pixbit-d 1 0 0 30)
                   (pixbit-d 1 1 0 40)
                    ))
              
                                
                      
                      
                      
                         
                   
                   


                                     



                


                     










  
  
  







