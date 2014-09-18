#lang racket

(require racket/gui
         "spreadsheet-editor.rkt")

(define cell-contents-hash (make-hash))
 
(define (get-cell-contents row column)
  (hash-ref cell-contents-hash (list row column) ""))
 
(define (set-cell-contents! row column contents)
  (hash-set! cell-contents-hash (list row column) contents))

(define (get-column-label i)
  (format "Column ~a" i))

(define (get-row-label i)
  (format "Row ~a" i))

(define (get-column-alignment i)
  'left)

(define (delete-row i)
  (let ((old-hash cell-contents-hash))
    (set! cell-contents-hash (make-hash))
    (hash-for-each
     old-hash
     (lambda (k v)
       (match-define (list row col) k)
       (cond
         ((< row i) (set-cell-contents! row col v))
         ((> row i) (set-cell-contents! (- row 1) col v)))))))

(define (add-row-before i)
   (let ((old-hash cell-contents-hash))
    (set! cell-contents-hash (make-hash))
    (hash-for-each
     old-hash
     (lambda (k v)
       (match-define (list row col) k)
       (cond
         ((< row i) (set-cell-contents! row col v))
         (else (set-cell-contents! (+ row 1) col v)))))))

(define frame (new frame% (label "Table editor")))

(void
 (new spreadsheet-editor%
      (parent frame)
      (n-rows 10000)
      (n-columns 100)
      (row-button-width 80)
      (get-row identity)
      (get-cell-contents get-cell-contents)
      (set-cell-contents! set-cell-contents!)
      (get-column-label get-column-label)
      (get-row-label get-row-label)
      (get-column-alignment get-column-alignment)
      (column-button-left-click
       (lambda (table-editor i)
         (lambda (self i)
           (message-box "Useful information"
                        (format "You just pressed column #~a" i)
                        frame '(ok)))))
      (row-button-left-click
       (lambda (table-editor)
         (lambda (self i)
           (define popup (new popup-menu% (title #f)))
           (new menu-item%
                (label "&Delete this row")
                (parent popup)
                (callback
                 (lambda (_ evt)
                   (delete-row i)
                   (send table-editor delete-row! i))))
           (new menu-item%
                (label "Add row &above")
                (parent popup)
                (callback
                 (lambda (_ evt)
                   (add-row-before i)
                   (send table-editor add-row-before! i))))
           (new menu-item%
                (label "Add row &below")
                (parent popup)
                (callback
                 (lambda (_ evt)
                   (add-row-before (+ 1 i))
                   (send table-editor add-row-before! i))))
           (send self popup-menu popup 0 (send self get-height)))))
      (editable? #t)))
 
(send frame resize 600 400)
(send frame show #t)
 