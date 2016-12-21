#lang racket

(require "window.rkt")

(struct board (width height values))

(define (create-matrix w h)
  (for/vector ([i h]) (make-vector w 0)))

(define (make-board width height)
  (board width height (create-matrix width height)))

(define (board-set! b x y value)
  (vector-set! (vector-ref (board-values b) y) x value))

(define (board-ref b x y)
  (vector-ref (vector-ref (board-values b) y) x))

(define (board-ref-safe b x y)
  (if (and
        (and (>= x 0) (< x (board-width b)))
        (and (>= y 0) (< y (board-height b))))
    (board-ref b x y) 0))

(define (board-swap!)
  (let ([tmp (current-read-board)])
    (current-read-board (current-write-board))
    (current-write-board tmp)))

(define (board->string b)
  (let ([out (open-output-string)])
    (for ([y (board-height b)])
      (for ([x (board-width b)])
        (if (= 1 (board-ref b x y))
          (display "o")
          (display " ")))
      (display "\n"))
    (get-output-string out)))

(define (neighbors b x y)
  (let ([r ((curry board-ref-safe) b)]
        [x1 (- x 1)] [x2 x] [x3 (+ x 1)]
        [y1 (- y 1)] [y2 y] [y3 (+ y 1)])
    (apply + (list
               (r x1 y1) (r x2 y1) (r x3 y1)
               (r x1 y2)           (r x3 y2)
               (r x1 y3) (r x2 y3) (r x3 y3)))))

(define (tick)
  (let ([r (current-read-board)]
        [w (current-write-board)])
    (for* ([y (in-range (board-height r))]
           [x (in-range (board-width r))])
      (let* ([alive? (= 1 (board-ref r x y))]
             [ns (neighbors r x y)]
             [new (or (and alive? (or (= ns 2) (= ns 3)))
                      (and (not alive?) (= ns 3)))])
        (board-set! w x y (if new 1 0)))))
  (board-swap!))

;; setup initial state
(define width 200)
(define height 100)
(define scale 8)

(define current-read-board (make-parameter (make-board width height)))
(define current-write-board (make-parameter (make-board width height)))

; seed
(let ([b (current-write-board)])
  (for* ([y (in-range (board-height b))]
         [x (in-range (board-width b))])
    (board-set! (current-write-board) x y (random 2)))
  (board-swap!))

(define window (create-window "Game of Life" width height scale))
(thread (lambda ()
          (let loop ()
            (let ([b (current-read-board)])
              (for* ([y (board-height b)]
                     [x (board-width b)])
                (send window set x y (= 1 (board-ref b x y))))
              (send window refresh)
              (sleep 0.5)
              (tick)
              (loop)))))
