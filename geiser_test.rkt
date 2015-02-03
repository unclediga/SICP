#lang racket

;; test file for Emacs Geiser mode 
;; working with embedded pictures

(require racket/draw slideshow/pict)
(define bart (bitmap (make-object bitmap% "bart.png")))
(frame (scale bart 0.7) )
