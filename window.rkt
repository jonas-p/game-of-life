#lang racket/gui

(provide create-window)

(define fg-color (make-object color% 0 0 0 1.0))
(define bg-color (make-object color% 255 255 255 1.0))

(define window%
  (class object%
    (init-field title
                width
                height
                scale)

    (define frame
      (new frame%
           [label title]
           [width (* scale width)]
           [height (* scale height)]
           [style '(no-resize-border)]))

    (define bitmap (make-object bitmap% width height #f #t))
    (define bitmap-dc (send bitmap make-dc))

    (define canvas
      (new canvas%
           [parent frame]
           [paint-callback
             (lambda (canvas dc)
               (send dc set-scale scale scale)
               (send dc draw-bitmap bitmap 0 0))]))

    (send frame show #t)

    (define/public (set x y alive?)
      (send bitmap-dc set-pixel x y (if alive? fg-color bg-color)))

    (define/public (refresh)
      (send canvas refresh-now))

    (super-new)))

(define (create-window title width height scale)
  (new window%
       [title title]
       [width width] [height height]
       [scale scale]))
