#lang racket/gui

(require racket/date)
(require json)

(define (get-config path)
  (let ((config-hash (string->jsexpr (file->string path))))
    (λ (option)
      (hash-ref config-hash option))))

(define config (get-config "glock.json"))

(define make-color ((curry make-object) color%))
(define inactive (apply make-color (config 'inactive)))
(define active (apply make-color (config 'active)))
    
(define frame (new frame%
                   [label "Glock"]
                   [width (+ (* (config 'margin) 2)
                             (* (config 'size) 8)
                             (* (config 'offset) 8)
                             5)]
                   [height (+ (* (config 'margin) 2)
                              (* (config 'size) 12)
                              (* (config 'offset) 12)
                              28)]
                   [style '(no-resize-border
                            float
                            metal
                            )]))

(define dc (send (new canvas%
                      [parent frame]) get-dc))

(send frame show #t)
(sleep/yield 0.1)

(send dc set-pen "white" 1 'transparent)

(define (draw-clock time)
  (define (draw-rect x y color percent)
    (letrec ((size (config 'size))
             (margin (config 'margin))
             (offset (config 'offset))
             (height (* size percent)))
      
      (define (get-pos i)
        (+ margin (* i (+ size offset))))
  
      (send dc set-brush color 'solid)
      (send dc draw-rectangle
            (get-pos x)
            (+ (get-pos y) (- size height))
            size
            height)))
  
  (define (draw-hours time)
    (for ((x 3))
      (for ((y 12))
        (let ((box-hour (+ (* x 12) (- 11 y))))
          (draw-rect x y
                     (if (> (date-hour time) box-hour) active inactive)
                     1.0)
          (when (= (date-hour time) box-hour)
            (draw-rect x y
                       active
                       (/ (date-minute time) 60)))))))
  
  (define (draw-minutes time)
    (for ((x 5))
      (for ((y 12))
        (let ((box-minute (+ x (* (- 11 y) 5))))
          (draw-rect (+ x 3) y
                     (if (> (date-minute time) box-minute) active inactive)
                     1.0)
          (when (= (date-minute time) box-minute)
            (draw-rect (+ x 3)
                       y
                       active
                       (/ (date-second time) 60)))))))
  
  (draw-hours time)
  (draw-minutes time))

(draw-clock (current-date))

(define timer
  (new timer%
       (interval 1000)
       (notify-callback
        (λ ()
          (draw-clock (current-date))))))