#lang racket
(require "TDAPixel.rkt")
(require "TDAImagen.rkt")
(require "TDAPixeles.rkt")
(require "TDAColor.rkt")



(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)
 ))

(define img2(image 2 2
                   (pixhex-d 0 0 "#FF0000" 10)
                   (pixhex-d 0 1 "#00FF00" 20)
                   (pixhex-d 1 0 "#0000FF" 30)
                   (pixhex-d 1 1 "#FFFFFF" 40)
                    ))

(define img3(image 2 2
                   (pixbit-d 0 0 1 10)
                   (pixbit-d 0 1 1 20)
                   (pixbit-d 1 0 0 30)
                   (pixbit-d 1 1 0 40)
                    ))


(define edit(lambda (filtro imagen)
              (list (getWidth imagen)(getHeight imagen)(map-px filtro (getPixeles imagen)))
  ))

(define invertColorBit(lambda(pixel)
                        (pixbit-d (getPosX pixel)
                                  (getPosY pixel)
                                  (if (=(getBit pixel)1)
                                      0
                                      1)
                                  (getDepth_Bit pixel))
                        ))

(define invertColorRGB(lambda(pixel)
                        (pixrgb-d (getPosX pixel)
                                  (getPosY pixel)
                                  (opuesto(getRed pixel))
                                  (opuesto(getGreen pixel))
                                  (opuesto(getBlue pixel))
                                  (getDepth_RGB pixel))
                        ))
                                  
                                  
(define map-px(lambda(filtro pixeles)
                (map filtro pixeles)
                ))

(define opuesto(lambda(RGB)
                 (-(- 256 RGB)1)
                 ))


;(define compress(lambda(imagen)
                  














                   