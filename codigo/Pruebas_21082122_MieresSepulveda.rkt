#lang racket
(require "TDAPixel_21082122_MieresSepulveda.rkt")
(require "TDAImagen_21082122_MieresSepulveda.rkt")
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "TDAColor_21082122_MieresSepulveda.rkt")
(require "CodigoFuente_21082122_MieresSepulveda.rkt")


;---------------------------------------------------------Archivo de Pruebas--------------------------------------------------------;

(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)
 ))


(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255)
 ))


(define img3 (imgRGB->imgHex img1))


(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f

(flipH img1)
(flipH img2)
(flipH img3)

(flipV img1)
(flipV img2)
(flipV img3)

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen

(histogram img1)
(histogram img2)
(histogram img3)
(histogram img4)
(histogram img5)
(histogram img6)
(histogram img7)

(define img18 (rotate90 img1))
(define img19 (rotate90 img2))
(define img20 (rotate90 img3))
(define img21 (rotate90 img4))
(define img22 (rotate90 img5))
(define img23 (rotate90 img6))
(define img24 (rotate90 img7))

(define img8 (compress img1))
(define img9 (compress img2))
(define img10 (compress img3))
(define img11 (compress img4))
(define img12 (compress img5))
(define img13 (compress img6))
(define img14 (compress img7))

(compressed? img8)  ; la respuesta debería ser #t
(compressed? img9)  ; la respuesta debería ser #t
(compressed? img10)  ; la respuesta debería ser #t
(compressed? img11)  ; la respuesta debería ser #t
(compressed? img12)  ; la respuesta debería ser #t
(compressed? img13)  ; la respuesta debería ser #t
(compressed? img14)  ; la respuesta debería ser #t

(define img15 (edit invertColorBit img2))
(define img16 (edit invertColorRGB img1))
