#lang racket
(require "TDAPixeles.rkt")
(require "TDAPixel.rkt")
(require "TDAColor.rkt")
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
(provide invertColorBit)
(provide invertColorRGB)



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
                      (if (= (* (getWidth imagen) (getHeight imagen))(n_componentes?(getPixeles imagen)))
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


(define rotate90(lambda(imagen)
                  (cond ((hexmap? imagen)(list (getHeight imagen)(getWidth imagen)(rotateHex(getWidth imagen)(getPixeles imagen))))
                        ((bitmap? imagen)(list (getHeight imagen)(getWidth imagen)(rotateBit(getWidth imagen)(getPixeles imagen))))
                        ((pixmap? imagen)(list (getHeight imagen)(getWidth imagen)(rotateRGB(getWidth imagen)(getPixeles imagen))))
                         )
                  ))


(define edit(lambda(filtro imagen)
              (list (getWidth imagen)(getHeight imagen)(filtro imagen))
              ))

(define invertColorBit(lambda(imagen)
                        (cond ((bitmap? imagen)(invertBit (getPixeles imagen)))
                              (else "La imagen ingresadad no corresponde al tipo de filtro que se desea aplicar."))
                        ))

(define invertColorRGB(lambda(imagen)
                        (cond ((pixmap? imagen)(invertRGB (getPixeles imagen)))
                              (else "La imagen ingresadad no corresponde al tipo de filtro que se desea aplicar."))
                        ))
;--------------------------------------------------------OTRAS OPERACIONES-----------------------------------------------------;

(define histogram(lambda(imagen)
                   (case (histogramCase imagen)
                     [(0)(hexHistogram (coloresHex(getPixeles imagen))(car(coloresHex(getPixeles imagen))))]
                     [(1)(bitHistogram (getPixeles imagen)(getWidth imagen)(getHeight imagen))]
                     [(2)(rgbHistogram (coloresRgb(getPixeles imagen))(car(coloresRgb(getPixeles imagen))))]
                    )
                   ))

(define bitHistogram(lambda(pixeles ancho alto)
                         (define bitHistogramInt(lambda(pixeles ancho alto sumaBit)
                                               (if (null? pixeles)
                                                   (list(list 0 sumaBit)(list 1 (-(* ancho alto)sumaBit)))
                                                   (cond ((=(getBit(getPixel pixeles))0) (bitHistogram (cdr pixeles) ancho alto (+ sumaBit 1)))
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

(define histogramCase(lambda(imagen)
                       (cond ((hexmap? imagen) 0)
                             ((bitmap? imagen) 1)
                             ((pixmap? imagen) 2)
                             )
                       ))




                                     



                


                     










  
  
  






