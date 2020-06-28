#lang racket/gui

(require racket/date)
(require json)

(define inactive (make-object color% 100 100 100))
(define active (make-object color% 255 255 255))

(define rect-size 20)
(define rect-offset 5)
(define clock-area-offset 5)

(let ((config (string->jsexpr (file->string "glock.json"))))
  (set! rect-size (hash-ref config 'size))
  (set! rect-offset (hash-ref config 'offset))
  (set! clock-area-offset (hash-ref config 'offset))

  (let ((config-active (hash-ref config 'active))
        (config-inactive (hash-ref config 'inactive)))
    (if (list? config-active)
        (set! active (make-object color%
                       (first config-active)
                       (second config-active)
                       (third config-active)))
        (set! active config-active))
    (if (list? config-inactive)
        (set! inactive (make-object color%
                       (first config-inactive)
                       (second config-inactive)
                       (third config-inactive)))
        (set! inactive config-inactive))))

(define frame-width
  (+ clock-area-offset
     (* rect-size 8)
     (* rect-offset 8)
     6))

(define frame-height
  (+ clock-area-offset
     (* rect-size 12)
     (* rect-offset 12)
     29))

(define frame (new frame%
                   [label "Glock"]
                   [width frame-width]
                   [height frame-height]
                   [style '(no-resize-border
                            float
                            metal
                            )]))
(define canvas (new canvas%
                    [parent frame]))
(define dc (send canvas get-dc))

(send frame show #t)
(sleep/yield 0.1)

(send dc set-pen "white" 1 'transparent)
(send dc set-brush active 'solid)

(define (draw-clock time)
  (define (draw-hours hour)
    (for ((x 3))
      (for ((y 12))
        (if (> hour (+ (* x 12) (- 11 y)))
            (send dc set-brush active 'solid)
            (send dc set-brush inactive 'solid))
        (send dc draw-rectangle (+ (* x (+ rect-size rect-offset)) clock-area-offset)
                                (+ (* y (+ rect-size rect-offset)) clock-area-offset)
                                rect-size rect-size))))

  (define (draw-minutes minute)
    (for ((x 5))
      (for ((y 12))
        (let ((box-minute (+ x (* (- 11 y) 5))))
          (if (> minute box-minute)
              (send dc set-brush active 'solid)
              (send dc set-brush inactive 'solid))
          (send dc draw-rectangle (+ (* x (+ rect-size rect-offset))
                                     (+ clock-area-offset (* rect-offset 3) (* rect-size 3)))
                (+ (* y (+ rect-size rect-offset)) clock-area-offset)
                rect-size rect-size)))))

  (define (draw-seconds time)  
    (define minute (date-minute time))
    (define second (date-second time))
    
    (define y (- 11 (floor (/ minute 5))))
    (define x (modulo minute 5))
    
    (define height (floor (* rect-size (/ second 60))))
    
    (send dc set-brush active 'solid)
    (send dc draw-rectangle (+ (* x (+ rect-size rect-offset))
                                     (+ clock-area-offset (* rect-offset 3) (* rect-size 3)))
                (+ (- rect-size height) (* y (+ rect-size rect-offset)) clock-area-offset)
                rect-size height))
  
  (draw-hours (date-hour time))
  (draw-minutes (date-minute time))
  (draw-seconds time))

(define (loop)
  (draw-clock (current-date))
  (sleep/yield 1)
  (loop))

(loop)