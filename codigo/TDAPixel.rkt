#lang racket
(provide pixrgb-d)
(provide pixbit-d)
(provide pixhex-d)
(provide getPosX)
(provide getPosY)
(provide getPixel)
(provide getDepth_Bit)
(provide getDepth_RGB)
(provide getDepth_Hex)
(provide esBit?)
(provide esRGB?)
(provide esHex?)
(provide invertirBitH)
(provide invertirBitV)
(provide invertirRGBH)
(provide invertirRGBV)
(provide invertirHexH)
(provide invertirHexV)
(provide newPosX)
(provide newPosY)
(provide filtro-px)
(provide n_componentes?)





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

(define getDepth_Bit(lambda(pixel)
                      (if (esBit? pixel)
                          (fourth pixel)
                          "El pixel ingresado no es del tipo pixbit-d."
                          )
                      ))

                 
(define getDepth_RGB(lambda(pixel)
                      (if (esRGB? pixel)
                          (sixth pixel)
                          "El pixel ingresado no es del tipo pixrgb-d."
                          )
                      ))

(define getDepth_Hex(lambda(pixel)
                      (if (esHex? pixel)
                          (fourth pixel)
                          "El pixel ingresado no es del tipo pixhex-d."
                          )
                      ))

;Nombre: getBit.
;Descripción: Función extrae el 3er elemento de un pixel, bajo el contexto de la funcion bitmap? esta obtiene el bit almacenado en el pixel
;Dominio: Imagen.
;Recorrido: Bit 0 o 1.
;Tipo de recursión: No aplica.
(define getBit(lambda(pixel)
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



;las 2 siguientes dejan los colores en solo una lista
(define coloresHex(lambda(pixeles)
                 (if (null? pixeles)
                     null
                     (cons (getHex(getPixel pixeles)) (coloresHex(cdr pixeles)))
                 )))

(define coloresRgb(lambda(pixeles)
                    (if (null? pixeles)
                        null
                        (cons  (list (getRed(getPixel pixeles))
                                    (getGreen(getPixel pixeles))
                                    (getBlue(getPixel pixeles)))
                              (coloresRgb(cdr pixeles)))
                        )))

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

;Descripción: Función que determina si un el color ingresado corresponde a la notación hexadecimal.
;Dominio: Color.
;Recorrido: Booleano.
;Tipo de recursión: No aplica.
(define hexadecimal?(
                     lambda(color)
                      (string? color)))

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

(define filtro-px(lambda(filtro pixeles)
                   (filter filtro pixeles)))

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

;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_componentes?(lambda (pixel)
                        (cond ((null? pixel) 0)
                              (else (+ 1 (n_componentes?(cdr pixel)))))
                        ))

;Descripción: Función que determina el número de componentes en la estructura de un pixel.
;Dominio: pixél.
;Recorrido: N° de componentes en el pixel.
;Tipo de recursión: Recursion Natural
(define n_pixeles?(lambda (pixeles)
                        (cond ((null? pixeles) 0)
                              (else (+ 1 (n_pixeles?(cdr pixeles)))))
                    ))

