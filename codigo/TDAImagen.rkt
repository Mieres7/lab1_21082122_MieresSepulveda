#lang racket
(provide image)
(provide getWidth)
(provide getHeight)
(provide pixmap?)
(provide bitmap?)
(provide hexmap?)
(provide compressed?)
(provide flipH)
(provide flipV)
(provide imgRGB->imgHex)
(provide nuevaDim)
(provide crop)
(provide filtroCrop)
(provide rotate90)
(provide histogram)
(provide bitHistogram)
(provide rgbHistogram)
(provide hexHistogram)
(provide histogramCase)
(provide edit)
(provide compress)
(require "TDAPixel.rkt")
(require "TDAPixeles.rkt")
(require "TDAColor.rkt")



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

(define getWidth(lambda(image)
                  (car image)))

(define getHeight(lambda(image)
                   (car(cdr image))))



;----------------------------------------------------------PERTENENCIA----------------------------------------------------------;

;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixrgb-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define pixmap?(
                lambda(image)
                 (if (= (n_componentes?(getPixel(getPixeles image)))6)
                                 #t
                                 #f)))
                 
;Descripción: Función que determina si una imagen corresponde a un conjunto de pixeles del tipo pixbit-d.
;Dominio: Imagen.
;Recorrido: Boleano.
;Tipo de recursión: No aplica.
(define bitmap?(
                lambda(image)
                 (cond((= (getBit(getPixel(getPixeles image))) 1) '#t)
                      ((= (getBit(getPixel(getPixeles image))) 0) '#t)
                      ((= (n_componentes?(getPixel(getPixeles image))) 6) '#f)
                      )
                 ))

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

;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Documentacion
(define flipH (lambda (image)
                (cond ((bitmap? image) (list (getWidth image)(getHeight image)(invertirBitH(getWidth image)(getPixeles image))))
                      ((pixmap? image) (list (getWidth image)(getHeight image)(invertirRGBH(getWidth image)(getPixeles image))))
                      ((hexmap? image) (list (getWidth image)(getHeight image)(invertirHexH(getWidth image)(getPixeles image))))

                 )
               )
)

(define flipV (lambda (image)
                (cond ((bitmap? image) (list (getWidth image)(getHeight image)(invertirBitV(getHeight image)(getPixeles image))))
                      ((pixmap? image) (list (getWidth image)(getHeight image)(invertirRGBV(getHeight image)(getPixeles image))))
                      ((hexmap? image) (list (getWidth image)(getHeight image)(invertirHexV(getHeight image)(getPixeles image))))

                 )
               )
)

(define imgRGB->imgHex(lambda (image)
                        (list (getWidth image)(getHeight image)(convert(getPixeles image)))
                        ))

(define crop(lambda(image x1 y1 x2 y2)
              (list (nuevaDim x1 x2)(nuevaDim y1 y2)(filtroCrop image (getPixel(getPixeles image)) x1 y1 x2 y2)
                     )
              ))

(define nuevaDim(lambda(d1 d2)
                  (+(- d2 d1)1)
                  ))

(define filtroCrop(lambda(image pixel x1 y1 x2 y2)
                    (filtro-px(lambda(pixel)(and
                               (>= (getPosX pixel) x1)(<= (getPosX pixel) x2)
                               (>= (getPosY pixel) y1)(<= (getPosY pixel) y2)
                               ))(getPixeles image)
  )
                    ))


(define rotate90(lambda(image)
                  (cond ((hexmap? image)(list (getHeight image)(getWidth image)(rotateHex(getWidth image)(getPixeles image))))
                        ((bitmap? image)(list (getHeight image)(getWidth image)(rotateBit(getWidth image)(getPixeles image))))
                        ((pixmap? image)(list (getHeight image)(getWidth image)(rotateRGB(getWidth image)(getPixeles image))))
                         )
                  ))


(define edit(lambda (filtro image)
              (list (getWidth image)(getHeight image)(map-px filtro (getPixeles image)))
  ))

(define invertColorBit(lambda(image)
                        (cond ((bitmap? image)(invertBit (getPixeles image)))
                              (else "La imagen ingresadad no corresponde al tipo de filtro que se desea aplicar."))
                        ))

(define invertColorRGB(lambda(image)
                        (cond ((pixmap? image)(invertRGB (getPixeles image)))
                              (else "La imagen ingresadad no corresponde al tipo de filtro que se desea aplicar."))
                        ))

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
                        ((pixmap? image) (list(delCommonRGB (detCommon(histogram image)) (getPixeles image))
                                              (commonDRGB(getPixeles image)(detCommon(histogram image)))
                                              (list(detCommon(histogram image)))
                                              (list (getWidth image)(getHeight image))
                                              ))
                        )
                  ))
;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

(define histogram(lambda(image)
                   (case (histogramCase image)
                     [(0)(hexHistogram (coloresHex(getPixeles image))(car(coloresHex(getPixeles image))))]
                     [(1)(bitHistogram (getPixeles image)(getWidth image)(getHeight image))]
                     [(2)(rgbHistogram (coloresRgb(getPixeles image))(car(coloresRgb(getPixeles image))))]
                    )
                   ))

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

(define histogramCase(lambda(image)
                       (cond ((hexmap? image) 0)
                             ((bitmap? image) 1)
                             ((pixmap? image) 2)
                             )
                       ))




                                     



                


                     










  
  
  







