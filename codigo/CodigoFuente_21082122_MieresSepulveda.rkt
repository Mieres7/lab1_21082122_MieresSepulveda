#lang racket
(provide image)
(provide bitmap?)
(provide pixmap?)
(provide hexmap?)
(provide compressed?)
(provide flipH)
(provide flipV)
(provide crop)
(provide imgRGB->imgHex)
(provide histogram)
(provide rotate90)
(provide compress)
(provide edit)
(provide invertColorBit)
(provide invertColorRGB)
(require "TDAPixel_21082122_MieresSepulveda.rkt")
(require "TDAImagen_21082122_MieresSepulveda.rkt")
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "TDAColor_21082122_MieresSepulveda.rkt")



;Nombre: image.
;Descripción: Función constructura de imagenes a partir de una dimension dada (ancho y alto) y un conjunto de pixeles del tipo RGB-d, bit-d o hex-d.
;Dominio: Ancho, altura, conjunto de pixeles (pixrgb-d, pixbit-d, píxhex-d)
;Recorrido: Imagen.
;Tipo de recursión: No aplica.
(define image(lambda(ancho altura . pixeles)
               (if (= (* ancho altura)(n_componentes? pixeles))
                   (list ancho altura pixeles)
                   "Favor ingresar cantidad correcta de pixeles, o en su defecto la dimensión correcta de la imagen.")))


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


;Nombre: compressed?
;Descripción: Función que determina si una imagen está comprimida o no.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define compressed?(
                     lambda(image)
                      (if (= (* (getWidth image) (getHeight image))(n_componentes?(getPixeles image)))
                          #f
                          #t)))


;Nombre: flipH
;Descripción: Función que invierte horizontalmente una imagen.
;Dominio: Image
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica
(define flipH (lambda (image)
                (cond ((bitmap? image) (list (getWidth image)(getHeight image)(invertirBitH(getWidth image)(getPixeles image))))
                      ((pixmap? image) (list (getWidth image)(getHeight image)(invertirRGBH(getWidth image)(getPixeles image))))
                      ((hexmap? image) (list (getWidth image)(getHeight image)(invertirHexH(getWidth image)(getPixeles image))))

                 )
               )
)

;Nombre: flipV
;Descripción: Función que invierte verticalmente una imagen.
;Dominio: Image
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica
(define flipV (lambda (image)
                (cond ((bitmap? image) (list (getWidth image)(getHeight image)(invertirBitV(getHeight image)(getPixeles image))))
                      ((pixmap? image) (list (getWidth image)(getHeight image)(invertirRGBV(getHeight image)(getPixeles image))))
                      ((hexmap? image) (list (getWidth image)(getHeight image)(invertirHexV(getHeight image)(getPixeles image))))

                 )
               )
)

;Nombre: crop
;Descripción: Función que recorta una imagen a partir de un cuadrante.
;Dominio: Image X x1(int) X y1(int) X x2(int) X y2(int)
;Recorrido: iIamge
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define crop(lambda(image x1 y1 x2 y2)
              (list (nuevaDim x1 x2)(nuevaDim y1 y2)(filtroCrop image (getPixel(getPixeles image)) x1 y1 x2 y2)
                     )
              ))

;Nombre: imgRGB->imgHex
;Descripción: Transforma una imagen desde una representación RGB a una representación Hex.
;Dominio: Image.
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define imgRGB->imgHex(lambda (image)
                        (list (getWidth image)(getHeight image)(convert(getPixeles image)))
                        ))

;Nombre: histogram
;Descripción: Función que retorna un histograma de frecuencias en base a los colores.
;Dominio: Image. 
;Recorrido: histogram.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define histogram(lambda(image)
                   (case (histogramCase image)
                     [(0)(hexHistogram (coloresHex(getPixeles image))(car(coloresHex(getPixeles image))))]
                     [(1)(bitHistogram (getPixeles image)(getWidth image)(getHeight image))]
                     [(2)(rgbHistogram (coloresRgb(getPixeles image))(car(coloresRgb(getPixeles image))))]
                    )
                   ))


;Nombre: rotate90
;Descripción: Función que rota una imagen 90° a la derecha.
;Dominio: Image.
;Recorrido: Image.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define rotate90(lambda(image)
                  (cond ((hexmap? image)(list (getHeight image)(getWidth image)(rotateHex(getWidth image)(getPixeles image))))
                        ((bitmap? image)(list (getHeight image)(getWidth image)(rotateBit(getWidth image)(getPixeles image))))
                        ((pixmap? image)(list (getHeight image)(getWidth image)(rotateRGB(getWidth image)(getPixeles image))))
                         )
                  ))

;Nombre: compress
;Descripción: Función que comprime una imagen.
;Dominio: Image
;Recorrido: Image Compressed
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define compress(lambda(image)
                  (cond ((hexmap? image) (list(getWidth image)
                                              (getHeight image)
                                              (delCommonHex (detCommon(histogram image)) (getPixeles image))
                                              (commonDHex (getPixeles image)(detCommon(histogram image)))
                                              (detCommon(histogram image))
                                              
                                              ))
                        ((bitmap? image) (list (getWidth image)
                                               (getHeight image)
                                               (delCommonBit (detCommon(histogram image)) (getPixeles image))
                                               (commonDBit(getPixeles image)(detCommon(histogram image)))
                                               (detCommon(histogram image))
                                             
                                              ))
                        ((pixmap? image) (list (getWidth image)
                                               (getHeight image)
                                               (delCommonRGB (detCommon(histogram image)) (getPixeles image))
                                               (commonDRGB(getPixeles image)(detCommon(histogram image)))
                                               (detCommon(histogram image))
                                              
                                              ))
                        )
                  ))


;Nombre: edit
;Descripción: Funcion que permita aplicar funciones especiales a la imagenes.
;Dominio: filtro X Image.
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define edit(lambda (filtro image)
              (list (getWidth image)(getHeight image)(map-px filtro (getPixeles image)))
  ))


;Nombre: invertColorBit
;Descripción: Función que permite obtener un pixel del tipo pixbit, con el bit opuesto.
;Dominio: Pixel(pixbit-d)
;Recorrido:  Pixel(pixbit-d)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define invertColorBit(lambda(pixel)
                        (pixbit-d (getPosX pixel)
                                  (getPosY pixel)
                                  (if (=(getBit pixel)1)
                                      0
                                      1)
                                  (getDepth_Bit pixel))
                        ))

;Nombre: invertColorRGB
;Descripción: Funcion que permite obetener un pixel del tipo pixrgb, con el color simetricamente opuesto en cada canal de un pixel.
;Dominio: Pixel(pixrgb-d)
;Recorrido: Pixel(pixrgb-d)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define invertColorRGB(lambda(pixel)
                        (pixrgb-d (getPosX pixel)
                                  (getPosY pixel)
                                  (opuesto(getRed pixel))
                                  (opuesto(getGreen pixel))
                                  (opuesto(getBlue pixel))
                                  (getDepth_RGB pixel))
                        ))


(define image->string(lambda(image func)
                       (cond ((hexmap? image))
                             ((bitmap? image)(func(getPixeles image )(getWidth image)))
                             ((pixmap? image))
                             )
                       ))



(define pixbit->string(lambda(pixeles filas)
                        (if (null? pixeles)
                            null
                            (cons (if (= (getPosX(getPixel pixeles))(+ filas 1))
                                      

                                      )
                            
                                
                        
                          
                            


(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)
 ))






