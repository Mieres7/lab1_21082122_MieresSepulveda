#lang racket

(provide getPixeles)
(provide convert)
(provide n_pixeles?)
(provide rotateBit)
(provide rotateHex)
(provide rotateRGB)
(provide invertBit)
(provide invertRGB)
(provide commonDBit)
(provide commonDHex)
(provide commonDRGB)
(provide detCommon)
(provide delCommonHex)
(provide delCommonBit)
(provide delCommonRGB)
(require "TDAColor.rkt")
(require "TDAPixel.rkt")

; TDA - Pixeles

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;


;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getPixeles.
;Descripción: Funcion que retorna los pixeles de una imagen.
;Dominio: Imagen.
;Recorrido: Pixeles de Imagen.
;Tipo de recursion: No aplica
(define getPixeles(lambda(image)
                    (third image)))

(define commonDBit(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(= (getBit (getPixel pixeles)) common))
                                (cons(getDepth_Bit(getPixel pixeles))(commonDBit (cdr pixeles) common))
                                (commonDBit (cdr pixeles) common)
                                     )
                            )
                    ))

(define commonDRGB(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(equal? (getColorsRGB(getPixel pixeles)) common))
                                (cons(getDepth_RGB(getPixel pixeles))(commonDRGB (cdr pixeles) common))
                                (commonDRGB (cdr pixeles) common)
                                     )
                            )
                    ))

(define commonDHex(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(equal?(getHex (getPixel pixeles)) common))
                                (cons(getDepth_Hex(getPixel pixeles))(commonDHex(cdr pixeles) common))
                                (commonDHex (cdr pixeles) common)
                                     )
                            )
                    ))

(define detCommon(lambda(histogram)
                   (define detCommonEnv(lambda(histogram common)
                                         (if (null? histogram)
                                             (first common)
                                             (if (>(second common)(second(first histogram)))
                                                 (detCommonEnv (cdr histogram)common)
                                                 (detCommonEnv (cdr histogram) (first histogram))
                                                 
                                             )
                                         )))
                   (detCommonEnv histogram (first histogram))
                   ))


;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;


;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Modifica una lista de pixeles rgb a hex
(define convert(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (pixhex-d (getPosX(getPixel pixeles))
                                     (getPosY(getPixel pixeles))
                                     (RGBHex (getRed(getPixel pixeles))(getGreen(getPixel pixeles))(getBlue(getPixel pixeles)))
                                     (getDepth_RGB(getPixel pixeles)))
                           (convert(cdr pixeles))))))

(define rotateBit(lambda(filas pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixbit-d (getPosY(getPixel pixeles))
                                       (- (- filas 1)(getPosX(getPixel pixeles)))
                                       (getBit(getPixel pixeles))
                                       (getDepth_Bit(getPixel pixeles)))
                             (rotateBit filas (cdr pixeles)))
                       )))

(define rotateHex(lambda(filas pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixhex-d (getPosY(getPixel pixeles))
                                       (- (- filas 1)(getPosX(getPixel pixeles)))
                                       (getHex(getPixel pixeles))
                                       (getDepth_Hex(getPixel pixeles)))
                             (rotateHex filas (cdr pixeles)))
                       )))

(define rotateRGB(lambda(filas pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixrgb-d (getPosY(getPixel pixeles))
                                       (- (- filas 1)(getPosX(getPixel pixeles)))
                                       (getRed(getPixel pixeles))
                                       (getGreen(getPixel pixeles))
                                       (getBlue(getPixel pixeles))
                                       (getDepth_RGB(getPixel pixeles)))
                             (rotateRGB filas (cdr pixeles)))
                       )))


(define invertBit(lambda(pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixbit-d (getPosX(getPixel pixeles))
                                       (getPosY(getPixel pixeles))
                                       (if (=(getBit(getPixel pixeles))1)
                                           0
                                           1)
                                       (getDepth_Bit(getPixel pixeles)))
                             (invertBit (cdr pixeles)))
                       )
                   ))


(define invertRGB(lambda(pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixrgb-d (getPosX(getPixel pixeles))
                                       (getPosY(getPixel pixeles))
                                       (opuesto(getRed(getPixel pixeles)))
                                       (opuesto(getGreen(getPixel pixeles)))
                                       (opuesto(getBlue(getPixel pixeles)))
                                       (getDepth_RGB(getPixel pixeles)))
                             (invertRGB (cdr pixeles)))
                       )
                   ))

(define delCommonBit(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(=(getBit (getPixel pixeles)) common))
                                  (cons (getPixel pixeles)(delCommonBit common (cdr pixeles)))
                                  (delCommonBit common (cdr pixeles))
                       ))
                      ))
                   
(define delCommonRGB(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(equal? (getColorsRGB(getPixel pixeles)) common))
                           (cons (getPixel pixeles)(delCommonRGB common (cdr pixeles)))
                           (delCommonRGB common (cdr pixeles))
                           ))
                      ))


(define delCommonHex(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(equal?(getHex (getPixel pixeles)) common))
                                  (cons (getPixel pixeles)(delCommonHex common (cdr pixeles)))
                                  (delCommonHex common (cdr pixeles))
                       ))
                      ))
;-------------------------------------------------------OTRAS OPERACIONES------------------------------------------------------;


;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_pixeles?(lambda (pixeles)
                        (cond ((null? pixeles) 0)
                              (else (+ 1 (n_pixeles?(cdr pixeles)))))
                    ))