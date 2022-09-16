#lang racket
(require "TDAPixel.rkt")
(require "TDAColor.rkt")
(provide getPixeles)
(provide convert)
(provide n_pixeles?)
(provide rotateBit)
(provide rotateHex)
(provide rotateRGB)
(provide invertBit)
(provide invertRGB)


;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;


;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getPixeles.
;Descripción: Funcion que retorna los pixeles de una imagen.
;Dominio: Imagen.
;Recorrido: Pixeles de Imagen.
;Tipo de recursion: No aplica
(define getPixeles(lambda(imagen)(
                                  third imagen)))


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
;-------------------------------------------------------OTRAS OPERACIONES------------------------------------------------------;


;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_pixeles?(lambda (pixeles)
                        (cond ((null? pixeles) 0)
                              (else (+ 1 (n_pixeles?(cdr pixeles)))))
                    ))