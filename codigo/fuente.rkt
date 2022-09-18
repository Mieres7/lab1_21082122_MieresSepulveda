#lang racket
(require "TDAPixel.rkt")
(require "TDAImagen.rkt")
(require "TDAPixeles.rkt")
(require "TDAColor.rkt")



(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 255 0 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255  1)
                  ))

(define img2(image 2 2
                   (pixhex-d 0 0 "#FFFF00" 10)
                   (pixhex-d 0 1 "#FFFF00" 20)
                   (pixhex-d 1 0 "#0000FF" 30)
                   (pixhex-d 1 1 "#FFFFFF" 40)
                    ))

(define img3(image 2 2
                   (pixbit-d 0 0 1 10)
                   (pixbit-d 0 1 1 20)
                   (pixbit-d 1 0 1 30)
                   (pixbit-d 1 1 0 40)
                    ))





                                  
                                  





                  

                  











                   