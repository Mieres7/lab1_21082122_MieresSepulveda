#lang racket
(require "TDApixbit-d_21082122_MieresSepulveda.rkt")
(require "TDApixhex-d_21082122_MieresSepulveda.rkt")
(require "TDApixrgb-d_21082122_MieresSepulveda.rkt")
(require "TDAImagen_21082122_MieresSepulveda.rkt")
(require "TDAPixeles_21082122_MieresSepulveda.rkt")
(require "CodigoFuente_21082122_MieresSepulveda.rkt")


;---------------------------------------------------------Archivo de Pruebas--------------------------------------------------------;


;img1
;Creación de una imagen de 2 x 2 del tipo pixmap
(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255 1)
 ))

;img2
;Creación de una imagen de 2 x 2 del tipo bitmap
(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255))
 )

;img3
(define img3 (imgRGB->imgHex img1))


;img4
;Creación de una imagen de 4 x 2 del tipo pixmap
(define img4 (image 4 2
                  (pixrgb-d 0 0 64 47 64 10)
                  (pixrgb-d 0 1 47 64 47 20)
                  (pixrgb-d 0 2 89 23 89 30)
                  (pixrgb-d 0 3 64 47 64 40)
                  (pixrgb-d 1 0 170 12 170 50)
                  (pixrgb-d 1 1 12 170 12 60)
                  (pixrgb-d 1 2 64 47 64 70)
                  (pixrgb-d 1 3 130 220 130 80)
 ))

;img5
;Creación de una imagen de 2 x 3 del tipo bitmap
(define img5 (image 2 3
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 0 10)
                  (pixbit-d 1 1 1 30)
                  (pixbit-d 2 0 0 50)
                  (pixbit-d 2 1 1 60))
 )

;img6
(define img6 (imgRGB->imgHex img4))

(display "---Display de imágenes---")
(display (image->string img1 pixrgb->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img2 pixbit->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img3 pixhex->string)) ;imprimir una representación string de la imagen
(display "\n")

(display "\n")
(display "\n---Display de ejemplos---")

(display (image->string img4 pixrgb->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img5 pixbit->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img6 pixhex->string)) ;imprimir una representación string de la imagen
(display "\n")


(display "\n")
(display "\n---bitmap?---\n")
(bitmap? img1)           ; la respuesta debería ser #f
(bitmap? img2)           ; la respuesta debería ser #t
(bitmap? img3)           ; la respuesta debería ser #f
(bitmap? img4)           ; la respuesta debería ser #f
(bitmap? img5)           ; la respuesta debería ser #t
(bitmap? img6)           ; la respuesta debería ser #f

(display "\n")
(display "\n---pixmap?---\n")
(pixmap? img1)           ; la respuesta debería ser #t
(pixmap? img2)           ; la respuesta debería ser #f
(pixmap? img3)           ; la respuesta debería ser #f
(pixmap? img4)           ; la respuesta debería ser #t
(pixmap? img5)           ; la respuesta debería ser #f
(pixmap? img6)           ; la respuesta debería ser #f


(display "\n")
(display "\n---hexmap?---\n")
(hexmap? img1)           ; la respuesta debería ser #f
(hexmap? img2)           ; la respuesta debería ser #f
(hexmap? img3)           ; la respuesta debería ser #t
(hexmap? img4)           ; la respuesta debería ser #f
(hexmap? img5)           ; la respuesta debería ser #f
(hexmap? img6)           ; la respuesta debería ser #t

(display "\n")
(display "\n---compressed?---\n")
(compressed? img1)       ; la respuesta debería ser #f
(compressed? img2)       ; la respuesta debería ser #f
(compressed? img3)       ; la respuesta debería ser #f
(compressed? img4)       ; la respuesta debería ser #f
(compressed? img5)       ; la respuesta debería ser #f
(compressed? img6)       ; la respuesta debería ser #f


(display "\n")
(display "\n---flipH---\n")
(flipH img1)  ;gira una imagen horizontalmente
(flipH img2)  ;gira una imagen horizontalmente
(flipH img3)  ;gira una imagen horizontalmente
(flipH img4)  ;gira una imagen horizontalmente
(flipH img5)  ;gira una imagen horizontalmente
(flipH img6)  ;gira una imagen horizontalmente


(display "\n")
(display "\n---flipV---\n")
(flipV img1)  ;gira una imagen verticalmente
(flipV img2)  ;gira una imagen verticalmente
(flipV img3)  ;gira una imagen verticalmente
(flipV img4)  ;gira una imagen verticalmente
(flipV img5)  ;gira una imagen verticalmente
(flipV img6)  


(display "\n")
(define img7  (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img8  (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img9  (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img10 (crop img2 0 0 1 1)) ; debería retornar la misma imagen
(define img11 (crop img4 0 1 0 3)) ; debería retornar una imágen con tres pixeles
(define img12 (crop img5 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img13 (crop img4 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img14 (crop img5 0 0 1 1)) ; deberia retornar una imagen con cuatro pixeles


(display "\n")
(display "\n---histogram---\n")
(histogram img1)  ;retornar el histograma de una imagen
(histogram img2)  ;retornar el histograma de una imagen
(histogram img3)  ;retornar el histograma de una imagen
(histogram img4)  ;retornar el histograma de una imagen
(histogram img5)  ;retornar el histograma de una imagen
(histogram img6)  ;retornar el histograma de una imagen
(histogram img7)  ;retornar el histograma de una imagen
(histogram img8)  ;retornar el histograma de una imagen
(histogram img9)  ;retornar el histograma de una imagen
(histogram img10) ;retornar el histograma de una imagen
(histogram img12) ;retornar el histograma de una imagen
(histogram img13) ;retornar el histograma de una imagen
(histogram img14) ;retornar el histograma de una imagen


(define img15 (rotate90 img1))
(define img16 (rotate90 img2))
(define img17 (rotate90 img3))
(define img18 (rotate90 img4))
(define img19 (rotate90 img5))
(define img20 (rotate90 img6))
(define img21 (rotate90 img7))
(define img22 (rotate90 img8))
(define img23 (rotate90 img9))
(define img24 (rotate90 img10))
(define img25 (rotate90 img11))
(define img26 (rotate90 img12))
(define img27 (rotate90 img13))
(define img28 (rotate90 img14))

(define img29 (compress img1))
(define img30 (compress img2))
(define img31 (compress img3))
(define img32 (compress img4))
(define img33 (compress img5))
(define img34 (compress img6))
(define img35 (compress img7))
(define img36 (compress img8))
(define img37 (compress img9))
(define img38 (compress img10))
(define img39 (compress img11))
(define img40 (compress img12))
(define img41 (compress img13))
(define img42 (compress img14))

(display "\n")
(display "\n---compressed?---\n")
(compressed? img29)  ; la respuesta debería ser #t
(compressed? img30)  ; la respuesta debería ser #t
(compressed? img31)  ; la respuesta debería ser #t
(compressed? img32)  ; la respuesta debería ser #t
(compressed? img33)  ; la respuesta debería ser #t
(compressed? img34)  ; la respuesta debería ser #t
(compressed? img35)  ; la respuesta debería ser #t
(compressed? img36)  ; la respuesta debería ser #t
(compressed? img37)  ; la respuesta debería ser #t
(compressed? img38)  ; la respuesta debería ser #t
(compressed? img39)  ; la respuesta debería ser #t
(compressed? img40)  ; la respuesta debería ser #t
(compressed? img41)  ; la respuesta debería ser #t
(compressed? img42)  ; la respuesta debería ser #t


(define img43 (edit invertColorBit img2))
(define img44 (edit invertColorRGB img1))
(define img45 (edit invertColorRGB img4))
(define img46 (edit invertColorBit img5))
(define img47 (edit invertColorRGB img7))


(define img48 (edit (adjustChannel getR setR incChR) img1))
(define img49 (edit (adjustChannel getG setG incChG) img1))
(define img50 (edit (adjustChannel getB setB incChB) img1))
(define img51 (edit (adjustChannel getR setR incChR) img4))
(define img52 (edit (adjustChannel getB setB incChB) img4))
(define img53 (edit (adjustChannel getD setD incChD) img4))

(display "\n")
(display "\n---image->string (imagenes no comprimidas)---\n")
(display (image->string img1 pixrgb->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img2 pixbit->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img3 pixhex->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img4 pixrgb->string))  ;imprimir una representación string de la imagen
(display "\n") 
(display (image->string img5 pixbit->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img6 pixhex->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img7 pixrgb->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img8 pixbit->string))  ;imprimir una representación string de la imagen
(display "\n\n") 
(display (image->string img9 pixrgb->string))  ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img10 pixbit->string)) ;imprimir una representación string de la imagen
(display "\n\n")
(display (image->string img11 pixrgb->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img12 pixbit->string)) ;imprimir una representación string de la imagen
(display "\n\n")
(display (image->string img13 pixrgb->string)) ;imprimir una representación string de la imagen
(display "\n")
(display (image->string img14 pixbit->string)) ;imprimir una representación string de la imagen