#lang racket
(provide getPixeles)
(provide convert)
(provide n_pixeles?)
(provide rotateBit)
(provide rotateHex)
(provide rotateRGB)
(provide commonDBit)
(provide commonDHex)
(provide commonDRGB)
(provide detCommon)
(provide delCommonHex)
(provide delCommonBit)
(provide delCommonRGB)
(provide invertirBitH)
(provide invertirBitV)
(provide invertirRGBH)
(provide invertirRGBV)
(provide invertirHexH)
(provide invertirHexV)
(provide filtro-px)
(provide map-px)
(require "TDAColor_21082122_MieresSepulveda.rkt")
(require "TDAPixel_21082122_MieresSepulveda.rkt")


;---------------------------------------------------------TDA - Pixeles--------------------------------------------------------;

; Implementación del TDA Pixeles.
; Representación: List X Pixel.

;---------------------------------------------------------CONSTRUCTORES--------------------------------------------------------;

;----------------------------------------------------------SELECTORES----------------------------------------------------------;

;Nombre: getPixeles.
;Descripción: Funcion que retorna los pixeles de una imagen.
;Dominio: Image.
;Recorrido: Pixeles(list)
;Tipo de recursion: No aplica
(define getPixeles(lambda(image)
                    (third image)))


;Nombre: commonDBit
;Descripción: Función que retorna la profundidad con mas frecuencia de una lista de pixeles, perteneciente a una imagen del tipo bitmap.
;Dominio: Pixeles(list) X Color(int)
;Recorrido: Depth(int)
;Tipo de recursión: Recursión natural. Es necesaria, puesto que  se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define commonDBit(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(= (getBit (getPixel pixeles)) common))
                                (cons(getDepth_Bit(getPixel pixeles))(commonDBit (cdr pixeles) common))
                                (commonDBit (cdr pixeles) common)
                                     )
                            )
                    ))


;Nombre: commonDRGB
;Descripción: Función que retorna la profundidad con mas frecuencia de una lista de pixeles, perteneciente a una imagen del tipo pixmap.
;Dominio: Pixeles(list) X Color((Red(int) X Green(int) X Blue(int))
;Recorrido: Depth(int)
;Tipo de recursión: Recursión natural. Es necesaria, puesto que  se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define commonDRGB(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(equal? (getColorsRGB(getPixel pixeles)) common))
                                (cons(getDepth_RGB(getPixel pixeles))(commonDRGB (cdr pixeles) common))
                                (commonDRGB (cdr pixeles) common)
                                     )
                            )
                    ))

;Nombre: commonDHex
;Descripción: Función que retorna la profundidad con mas frecuencia de una lista de pixeles, perteneciente a una imagen del tipo hexmap.
;Dominio: Pixeles(list) X Color(str)
;Recorrido: Depth(int)
;Tipo de recursión: Recursión natural. Es necesaria, puesto que  se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define commonDHex(lambda(pixeles common)
                        (if (null? pixeles)
                            null
                            (if (not(equal?(getHex (getPixel pixeles)) common))
                                (cons(getDepth_Hex(getPixel pixeles))(commonDHex(cdr pixeles) common))
                                (commonDHex (cdr pixeles) common)
                                     )
                            )
                    ))


;Nombre: detCommon
;Descripción: Función que determina el color con mas frecuencia dentro de una imagen.
;Dominio: Histograma de una imagen -> Color(int) | Color(str) | Color((Red(int) X Green(int) X Blue(int)) X Cantidad(int)
;Recorrido: Color(int) | Color(str) | Color((Red(int) X Green(int) X Blue(int))
;Tipo de recursión: Recursión de Cola. Es necesaria, puesto que una vez encontrado el color mas frecuente, es necesario retornar
;                   la caracteristica del color, y no la cantidad.
;Estrategia: No aplica.
(define detCommon(lambda(histogram)
                   (define detCommonEnv(lambda(histogram common)
                                         (if (null? histogram)
                                             (first common)
                                             (if (>(second common)(second(first histogram)))
                                                 (detCommonEnv (cdr histogram)  common)
                                                 (detCommonEnv (cdr histogram) (first histogram))
                                                 
                                             )
                                         )))
                   (detCommonEnv histogram (first histogram))
                   ))


;----------------------------------------------------------PERTENENCIA---------------------------------------------------------;


;---------------------------------------------------------MODIFICADORES--------------------------------------------------------;

;Nombre: convert
;Descripción: Función que convierte una lista de pixeles del tipo RGB al tipo hexadecimal.
;Dominio: Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define convert(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (pixhex-d (getPosX(getPixel pixeles))
                                     (getPosY(getPixel pixeles))
                                     (RGBHex (getRed(getPixel pixeles))(getGreen(getPixel pixeles))(getBlue(getPixel pixeles)))
                                     (getDepth_RGB(getPixel pixeles)))
                           (convert(cdr pixeles))))))


;Nombre: rotateBit
;Descripción: Función que rota una lista de pixeles del tipo bit.
;Dominio: Filas(int) X pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define rotateBit(lambda(filas pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixbit-d (getPosY(getPixel pixeles))
                                       (- (- filas 1)(getPosX(getPixel pixeles)))
                                       (getBit(getPixel pixeles))
                                       (getDepth_Bit(getPixel pixeles)))
                             (rotateBit filas (cdr pixeles)))
                       )))


;Nombre: rotateHex
;Descripción: Función que rota una lista de pixeles del tipo hexadecimal.
;Dominio: Filas(int) X pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define rotateHex(lambda(filas pixeles)
                   (if (null? pixeles)
                       null
                       (cons (pixhex-d (getPosY(getPixel pixeles))
                                       (- (- filas 1)(getPosX(getPixel pixeles)))
                                       (getHex(getPixel pixeles))
                                       (getDepth_Hex(getPixel pixeles)))
                             (rotateHex filas (cdr pixeles)))
                       )))


;Nombre: rotateRGB
;Descripción: Función que rota una lista de pixeles del tipo RGB.
;Dominio: Filas(int) X pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
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





;Nombre: delCommonBit
;Descripción: Función que elimina los pixeles del color mas frecuente perteneciente a una lista de pixeles del tipo bit.
;Dominio: Color(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa e eliminar en caso de cumplir la condición.
;Estrategia: No aplica.
(define delCommonBit(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(=(getBit (getPixel pixeles)) common))
                                  (cons (getPixel pixeles)(delCommonBit common (cdr pixeles)))
                                  (delCommonBit common (cdr pixeles))
                       ))
                      ))


;Nombre: delCommonRGB
;Descripción: Función que elimina los pixeles del color mas frecuente perteneciente a una lista de pixeles del tipo RGB.
;Dominio: Color(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa e eliminar en caso de cumplir la condición.
;Estrategia: No aplica.
(define delCommonRGB(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(equal? (getColorsRGB(getPixel pixeles)) common))
                           (cons (getPixel pixeles)(delCommonRGB common (cdr pixeles)))
                           (delCommonRGB common (cdr pixeles))
                           ))
                      ))


;Nombre: delCommonHex
;Descripción: Función que elimina los pixeles del color mas frecuente perteneciente a una lista de pixeles del tipo hexadecimal.
;Dominio: Color(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural. Es necesaria puesto que se debe recorrer la lista de pixeles completa e eliminar en caso de cumplir la condición.
;Estrategia: No aplica.
(define delCommonHex(lambda(common pixeles)
                   (if (null? pixeles)
                       null
                       (if (not(equal?(getHex (getPixel pixeles)) common))
                                  (cons (getPixel pixeles)(delCommonHex common (cdr pixeles)))
                                  (delCommonHex common (cdr pixeles))
                       ))
                      ))


;Nombre: invertirBitH
;Descripción: Función que invierte horizontalmente una lista de pixeles del tipo Bit.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define invertirBitH(lambda(ancho pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixbit-d (getPosX(getPixel pixeles))
                                      (newPosY ancho(getPixel pixeles))
                                      (getBit(getPixel pixeles))
                                      (getDepth_Bit(getPixel pixeles)))
                            (invertirBitH ancho (cdr pixeles))
                                  ))))


;Nombre: invertirRGBH
;Descripción: Función que invierte horizontalmente una lista de pixeles del tipo RGB.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
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


;Nombre: invertirHexH
;Descripción: Función que invierte horizontalmente una lista de pixeles del tipo hexadecimal.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define invertirHexH(lambda(ancho pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixhex-d (getPosX(getPixel pixeles))
                                      (newPosY ancho(getPixel pixeles))
                                      (getHex(getPixel pixeles))
                                      (getDepth_Hex(getPixel pixeles)))
                            (invertirHexH ancho (cdr pixeles))
                                  ))))


;Nombre: invertirBitV
;Descripción: Función que invierte verticalmente una lista de pixeles del tipo Bit.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define invertirBitV(lambda(altura pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixbit-d (newPosX altura (getPixel pixeles))
                                      (getPosY(getPixel pixeles))
                                      (getBit(getPixel pixeles))
                                      (getDepth_Bit(getPixel pixeles)))
                            (invertirBitV altura (cdr pixeles))
                                  ))))


;Nombre: invertirRGBV
;Descripción: Función que invierte verticalmente una lista de pixeles del tipo RGB.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
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


;Nombre: invertirHexV
;Descripción: Función que invierte verticalmente una lista de pixeles del tipo hexadecimal.
;Dominio: Ancho(int) X Pixeles(list)
;Recorrido: Pixeles(list)
;Tipo de recursión: Recursión natural, es necesario puesto que se debe recorrer la lista de pixeles completa.
;Estrategia: No aplica.
(define invertirHexV(lambda(altura pixeles)
                  (if (null? pixeles)
                      null
                      (cons (pixhex-d (newPosX altura (getPixel pixeles))
                                      (getPosY(getPixel pixeles))
                                      (getHex(getPixel pixeles))
                                      (getDepth_Hex(getPixel pixeles)))
                            (invertirHexV altura (cdr pixeles))
                                  ))))

;-------------------------------------------------------OTRAS OPERACIONES------------------------------------------------------;


;Descripción: Función que determina el número de pixeles de una lista de pixeles.
;Dominio: Pixeles(list)
;Recorrido: Cantidad(int)
;Tipo de recursión: Recursion Natural, puesto que es necesario recorrer la lista de pixeles completa.
;Estrategia: No apica.
(define n_pixeles?(lambda (pixeles)
                        (cond ((null? pixeles) 0)
                              (else (+ 1 (n_pixeles?(cdr pixeles)))))
                    ))

;Nombre: filtro-px
;Descripción: Función que aplica un filtro a una lista de pixeles.
;Dominio: filtro X pixeles(list)
;Recorrido: pixeles(list)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define filtro-px(lambda(filtro pixeles)
                   (filter filtro pixeles)))

;Nombre: map-px
;Descripción: Función que aplica un mapeo a una lista de pixeles.
;Dominio: filtro X pixeles(list)
;Recorrido: pixeles(list)
;Tipo de recursión: No aplica.
;Estrategia: No aplica.
(define map-px(lambda(filtro pixeles)
                (map filtro pixeles)
                ))