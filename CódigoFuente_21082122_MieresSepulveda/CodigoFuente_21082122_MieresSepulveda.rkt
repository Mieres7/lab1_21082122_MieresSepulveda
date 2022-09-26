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
(provide adjustChannel)
(provide image->string)
(require "TDAImagen_21082122_MieresSepulveda.rkt")
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "TDApixbit-d_21082122_MieresSepulveda.rkt")
(require "TDApixrgb-d_21082122_MieresSepulveda.rkt")
(require "TDApixhex-d_21082122_MieresSepulveda.rkt")




;Nombre: image.
;Descripción: Función constructura de imagenes a partir de una dimension dada (ancho y alto) y un conjunto de pixeles del tipo RGB-d, bit-d o hex-d.
;Dominio: Ancho, altura, conjunto de pixeles (pixrgb-d, pixbit-d, píxhex-d)
;Recorrido: Imagen.
;Tipo de recursión: No aplica.
(define image(lambda(ancho altura . pixeles)
               (if (integer? (caar pixeles))
                   (list ancho altura pixeles)
                   (list ancho altura (car pixeles)))
               ))
                   


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
(define compressed?(lambda(image)
                      (if (= (n_componentes? image)5)
                          #t
                          #f)
                     ))
                          
                          


;Nombre: flipH
;Descripción: Función que invierte horizontalmente una imagen.
;Dominio: Image
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica
(define flipH (lambda (imagen)
                (cond ((bitmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirBitH(getWidth imagen)(getPixeles imagen))))
                      ((pixmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirRGBH(getWidth imagen)(getPixeles imagen))))
                      ((hexmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirHexH(getWidth imagen)(getPixeles imagen))))

                 )
               )
)

;Nombre: flipV
;Descripción: Función que invierte verticalmente una imagen.
;Dominio: Image
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica
(define flipV (lambda (imagen)
                (cond ((bitmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirBitV(getHeight imagen)(getPixeles imagen))))
                      ((pixmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirRGBV(getHeight imagen)(getPixeles imagen))))
                      ((hexmap? imagen) (image (getWidth imagen)(getHeight imagen)(invertirHexV(getHeight imagen)(getPixeles imagen))))

                 )
               )
)

;Nombre: crop
;Descripción: Función que recorta una imagen a partir de un cuadrante.
;Dominio: Image X x1(int) X y1(int) X x2(int) X y2(int)
;Recorrido: iIamge
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define crop(lambda(imagen x1 y1 x2 y2)
              (image (nuevaDim x1 x2)(nuevaDim y1 y2)(filtroCrop imagen (getPixel(getPixeles imagen)) x1 y1 x2 y2)
                     )
              ))

;Nombre: imgRGB->imgHex
;Descripción: Transforma una imagen desde una representación RGB a una representación Hex.
;Dominio: Image.
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define imgRGB->imgHex(lambda (imagen)
                        (image (getWidth imagen)(getHeight imagen)(convert(getPixeles imagen)))
                        ))

;Nombre: histogram
;Descripción: Función que retorna un histograma de frecuencias en base a los colores.
;Dominio: Image. 
;Recorrido: histogram.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define histogram(lambda(imagen)
                   (case (histogramCase imagen)
                     [(0)(hexHistogram (coloresHex(getPixeles imagen))(car(coloresHex(getPixeles imagen))))]
                     [(1)(bitHistogram (getPixeles imagen)(getWidth imagen)(getHeight imagen))]
                     [(2)(rgbHistogram (coloresRgb(getPixeles imagen))(car(coloresRgb(getPixeles imagen))))]
                    )
                   ))


;Nombre: rotate90
;Descripción: Función que rota una imagen 90° a la derecha.
;Dominio: Image.
;Recorrido: Image.
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define rotate90(lambda(imagen)
                  (cond ((hexmap? imagen)(image (getHeight imagen)(getWidth imagen)(rotateHex(getWidth imagen)(getPixeles imagen))))
                        ((bitmap? imagen)(image (getHeight imagen)(getWidth imagen)(rotateBit(getWidth imagen)(getPixeles imagen))))
                        ((pixmap? imagen)(image (getHeight imagen)(getWidth imagen)(rotateRGB(getWidth imagen)(getPixeles imagen))))
                         )
                  ))

;Nombre: compress
;Descripción: Función que comprime una imagen.
;Dominio: Image
;Recorrido: Image Compressed
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define compress(lambda(imagen)
                  (cond ((pixmap? imagen) (list (getWidth imagen)
                                                (getHeight imagen)
                                                (delCommonRGB (detCommon(histogram imagen)) (getPixeles imagen))
                                                (commonDRGB(getPixeles imagen)(detCommon(histogram imagen)))
                                                (detCommon(histogram imagen))
                                              
                                              ))
                        ((hexmap? imagen) (list(getWidth imagen)
                                              (getHeight imagen)
                                              (delCommonHex (detCommon(histogram imagen)) (getPixeles imagen))
                                              (commonDHex (getPixeles imagen)(detCommon(histogram imagen)))
                                              (list(detCommon(histogram imagen)))
                                              
                                              ))
                        ((bitmap? imagen) (list (getWidth imagen)
                                                (getHeight imagen)
                                                (delCommonBit (detCommon(histogram imagen)) (getPixeles imagen))
                                                (commonDBit(getPixeles imagen)(detCommon(histogram imagen)))
                                                (list(detCommon(histogram imagen)))
                                             
                                              ))
                        )
                  ))


;Nombre: edit
;Descripción: Funcion que permita aplicar funciones especiales a la imagenes.
;Dominio: filtro X Image.
;Recorrido: Image
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define edit(lambda (filtro imagen)
              (image (getWidth imagen)(getHeight imagen)(map-px filtro (getPixeles imagen)))
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
                                  (opuesto(getR pixel))
                                  (opuesto(getG pixel))
                                  (opuesto(getB pixel))
                                  (getDepth_RGB pixel))
                        ))


;Nombre: adjustChannel
;Descripción: Función que permite modificar cualquier canal de una imagen con pixeles pixrgb-d.
;Dominio: f1 X f2 X f3 X Pixel(pixrgb-d)
;Recorrido: Image
;Recursión: No aplica.
;Estrategia: Currificación parcial.
(define adjustChannel(lambda(selector modificador operador)(lambda(pixel)(modificador pixel(operador(selector pixel))))))                     
                                

                            
;Nombre: image->string
;Descripción: Función que transforma una imagen de cualquier tipo a un string
;Dominio: Image X func
;Recorrido: String
;Recursión: No aplica.
;Estrategia: No aplica.
(define image->string(lambda(image func)
                       (cond ((pixmap? image)(func (getPixeles(imgRGB->imgHex image))))
                             ((hexmap? image)(func (getPixeles image)))
                             ((bitmap? image)(func (getPixeles image)))
                             )
                       ))
                        
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)
 ))
                    

                             