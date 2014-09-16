#lang racket

(require 
  racket/gui
  racket/draw
  data/gvector
  (planet williams/table-panel:1:2/table-panel))

(provide spreadsheet-editor%)

;; Helper functions
(define (find-max list get-value)
  (for/fold ((max-value 0)) ((x list))
    (max (get-value x) max-value)))

(define (get-visible-text-width dc str)
  (cond
    ((void? dc) 0)
    (else
     (define-values (w h a b) (send dc get-text-extent str))
     (inexact->exact w))))

(define (visible-column-contents-width spreadsheet-editor visible-rows column)
  (for/fold ((max-pixels 0))
    ((table-row visible-rows))
    (define value-str ((get-field get-cell-contents spreadsheet-editor) table-row column))
    (values (max max-pixels (get-visible-text-width (send spreadsheet-editor get-dc) value-str)))))

(define special-button%
  (class button%
    (init-field (on-left-click #f)
                (on-right-click #f)
                (index #f))
    (define/public (set-index idx)
      (set! index idx))
    (define/public (get-index)
      index)
    (super-new (horiz-margin 0)
               (vert-margin 0))
    (define/override (on-subwindow-event receiver event)
      (when on-right-click
        (when (equal? (send event get-event-type) 'right-down)
          (on-right-click receiver index)))
      (when on-left-click
        (when (equal? (send event get-event-type) 'left-down)
          (on-left-click receiver index))
        ))))

;; Panel with column buttons
(define my-horizontal-panel%
  (class horizontal-panel%
    (super-new)

    (define/override (container-size info)
      (values 0 (find-max info second)))
    
    (field (n-column-buttons 0))
    
    (define parent (send this get-parent))
    
    (define/override (on-size w h)
      (define total-length 0)
      (set! n-column-buttons 0)
      (define flag #f)
      (send this begin-container-sequence)
      (define visible-rows (send parent get-visible-rows))
      (define starting-column (get-field starting-column parent))

      ;; Delete buttons that are completely out of sight
      (for ((child (send this get-children))
            (i (in-naturals starting-column)))
        (define width-pixels
          (visible-column-contents-width
                parent visible-rows i))
        (send child min-width
              (max (send parent column-button-min-width i)
                   (+ width-pixels (* 2 (get-field horizontal-margin parent)))))
        (set! total-length (+ total-length (send child get-width)))
        (cond
          ((> total-length w)
           (cond
             (flag
              (send this delete-child child))
             (else
              (set! n-column-buttons (+ 1 n-column-buttons))
              (set! flag #t))))
          (else
           (set! n-column-buttons (+ 1 n-column-buttons)))))
      
      (define all-column-buttons (get-field all-column-buttons parent))
      
      ;; Add buttons till there is place on the panel
      (let loop ()
        (when (and (< total-length w)
                   (< (+ starting-column n-column-buttons) (vector-length all-column-buttons)))
          (define button (vector-ref all-column-buttons (+ starting-column n-column-buttons)))
          (define width-pixels
            (visible-column-contents-width
             parent visible-rows (+ starting-column n-column-buttons)))
          (send button min-width
                (max (send parent column-button-min-width (+ starting-column n-column-buttons))
                     (+ width-pixels (* 2 (get-field horizontal-margin parent)))))
          (send this add-child button)
          (set! n-column-buttons (+ 1 n-column-buttons))
          (set! total-length (+ total-length (send button get-width)))
          (loop)))
      (send this end-container-sequence))
    
    ))

;; Panel with row buttons
(define my-vertical-panel%
  (class vertical-panel%
    (super-new)
    (define/override (container-size info)
      (values 0 0))
    
    (init-field (button-left-click #f))
    
    (field (n-row-buttons 0))
    (field (row-buttons (gvector)))
    
    (define parent (send this get-parent))
    
    (define/public (reset-buttons)
      (set! row-buttons (gvector)))
    
    (define/override (on-size w h)
      (define total-length 0)
      (set! n-row-buttons 0)
      (send this begin-container-sequence)
      ;; Delete buttons that are completely out of sight
      (define flag #f)
      (define starting-row (get-field starting-row parent))
      (define n-rows (get-field n-rows parent))
      (for ((child (send this get-children))
            (i (in-naturals)))
        (set! total-length (+ total-length (send child get-height)))
        (cond
          ((>= (+ i starting-row) n-rows)
           (send this delete-child child))
          ((and (> total-length h) flag)
           (send this delete-child child))
          (else
           (when (> total-length h)
             (set! flag #t))
           ;; ... and relabel buttons that are visible
           (send child set-index (+ n-row-buttons starting-row))
           (send child set-label (number->string (+ 1 n-row-buttons starting-row)))
           (set! n-row-buttons (+ 1 n-row-buttons))
           )))
      ;; Add buttons till there is place on the panel
      (let loop ()
        (when (and (< total-length h)
                   (< (+ n-row-buttons starting-row) n-rows))
          (define button (void))
          (cond
            ((< n-row-buttons (gvector-count row-buttons))
             ;; Relabel and reuse a previously added and deleted button
             (set! button (gvector-ref row-buttons n-row-buttons))
             (send button set-index (+ n-row-buttons starting-row))
             (send button set-label (number->string (+ 1 n-row-buttons starting-row)))
             (send this add-child button))
            (else
             ;; Add a new button
             (set! button (new special-button%
                               (parent this)
                               (stretchable-width #t)
                               (index (+ n-row-buttons starting-row))
                               (label (number->string (+ 1 n-row-buttons starting-row)))
                               (on-left-click
                                (and button-left-click
                                     (button-left-click
                                      parent)))))
             (gvector-add! row-buttons button)))
          (set! n-row-buttons (+ 1 n-row-buttons))
          (set! total-length (+ total-length (send button get-height)))
          (loop)))
      (send this end-container-sequence)
      )))

;; Specialized canvas to draw the grid lines and cells' contents
(define my-editor-canvas%
  (class editor-canvas%
    (init-field (spreadsheet-editor (void)))
    (super-new)
    
    (define/override (on-paint)
      (super on-paint)
      (define dc (send this get-dc))
      (define-values (width height) (send dc get-size))
      (define border-x 0)
      (define border-y 0)
      
      (define n-column-buttons (send spreadsheet-editor n-column-buttons))
      (define n-row-buttons (send spreadsheet-editor n-row-buttons))
      (define starting-column (get-field starting-column spreadsheet-editor))
      
      (when (positive? n-column-buttons)
        (define border-column-button
          (send spreadsheet-editor get-column-button (+ starting-column n-column-buttons -1)))
        (set! border-x (+ (send border-column-button get-x)
                          (send border-column-button get-width))))
      (when (positive? n-row-buttons)
        (define border-row-button
          (send spreadsheet-editor get-row-button (- n-row-buttons 1)))
        (set! border-y (+ (send border-row-button get-y)
                          (send border-row-button get-height))))
      
      (define row-buttons (send spreadsheet-editor row-buttons))
      (define starting-row (get-field starting-row spreadsheet-editor))
      (define n-rows (get-field n-rows spreadsheet-editor))
      (define visible-column-buttons (send spreadsheet-editor visible-column-buttons))
      
      (for ((btn visible-column-buttons))
        (send dc draw-line
              (- (send btn get-x) 1) 0
              (- (send btn get-x) 1) (- border-y 1)))
      (send dc draw-line
              (- border-x 1) 0
              (- border-x 1) (- border-y 1))
      
      (for ((btn row-buttons)
            (row (in-range starting-row n-rows)))
        (send dc draw-line
              0 (- (send btn get-y) 1)
              (- border-x 1) (- (send btn get-y) 1)))
      (send dc draw-line
              0 (- border-y 1)
              (- border-x 1) (- border-y 1))
      
      (define horizontal-margin (get-field horizontal-margin spreadsheet-editor))
      
      (define editor (send this get-editor))
      
      (define text-snip-row (get-field text-snip-row editor))
      (define text-snip-column (get-field text-snip-column editor))
      
      (for ((row-btn row-buttons)
            (row (in-range starting-row n-rows)))
        (define table-row ((get-field get-row spreadsheet-editor) row))
        (for ((col-btn visible-column-buttons)
              (column (in-naturals starting-column)))
          (unless (and (equal? text-snip-row row)
                       (equal? text-snip-column column))
            (define x (send col-btn get-x))
            (define y (send row-btn get-y))
            (define w (send col-btn get-width))
            (define h (send row-btn get-height))
            (define contents ((get-field get-cell-contents spreadsheet-editor) table-row column))
            (define text-w (get-visible-text-width dc contents))
            (send dc set-clipping-rect x y w h)
            (when (<= (+ text-w (* 2 horizontal-margin)) w)
              (match ((get-field get-column-alignment spreadsheet-editor) column)
                ('right (set! x (+ x (- w text-w horizontal-margin))))
                ('left (set! x (+ x horizontal-margin)))))
            (send dc draw-text contents x y)
            (send dc set-clipping-region #f)))))
    ))

;; Specialized text% for editing the cell contents
(define my-text%
  (class text%
    (super-new)
    (init-field pasteboard)
    (define/override (on-focus on?)
      (unless on?
        (send pasteboard done-with-text-snip #t))
      (super on-focus on?))
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (define text-obj (get-field text-obj pasteboard))
      (cond
        ((equal? key-code 'escape)
         (send pasteboard done-with-text-snip #f))
        ((equal? key-code #\return)
         (send pasteboard done-with-text-snip #t))
        ((and (send event get-control-down)
              (equal? key-code #\c))
         (send text-obj copy))
        ((and (send event get-control-down)
              (equal? key-code #\v))
         (send text-obj paste))
        ((and (send event get-control-down)
              (equal? key-code #\x))
         (send text-obj cut))
        (else (super on-char event)))
      )))

;; Specialized pasteboard% to place the text snip on
(define my-pasteboard%
  (class pasteboard%
    (super-new)
    (init-field spreadsheet-editor)
    (define/augment (can-select? snip on?)
      #f)
    (define/augment (can-interactive-move? snip)
      #f)
    (define/augment (can-interactive-resize? snip)
      #f)
    
    ;; Transient text snip to edit cell contents
    (field (text-snip (void)))
    (field (text-obj (void)))
    (field (text-snip-row (void)))
    (field (text-snip-column (void)))
    
    (define/public (done-with-text-snip save-contents?)
      (define ts text-snip)
      (unless (void? ts)
        (let ((contents (send (send ts get-editor) get-text)))
          ;; void the text-snip to prevent another (done-with-text-snip) from on-focus
          (set! text-snip (void))
          (when save-contents?
            ((get-field set-cell-contents! spreadsheet-editor) text-snip-row text-snip-column contents))
          (set! text-snip-row (void))
          (set! text-snip-column (void))
          (send ts release-from-owner)
          (send (get-field editor-canvas spreadsheet-editor) refresh))))
    
    ;; Helper functions for table cell editing
    (define (detect-column-by-x x)
      (define-values (col _)
        (for/fold ((result (get-field starting-column spreadsheet-editor))
                   (length 0))
          ((button (send spreadsheet-editor visible-column-buttons)))
          #:break (<= x (+ length (send button get-width)))
          (values (+ result 1)
                  (+ length (send button get-width)))))
      (if (< col (get-field n-columns spreadsheet-editor)) col #f))
    
    (define (detect-row-by-y y)
      (define rownum
        (+ (get-field starting-row spreadsheet-editor)
           (floor (/ y (send (send spreadsheet-editor get-row-button 0) get-height)))))
      (if (< rownum (get-field n-rows spreadsheet-editor)) rownum #f))
    
    (define/override (on-default-event event)
      (define type (send event get-event-type))
      (cond
        ((and (equal? type 'left-down)
              (get-field editable? spreadsheet-editor)
              (not (zero? (get-field n-rows spreadsheet-editor))))
         (unless (void? text-snip)
           (done-with-text-snip #t))
         (define x (send event get-x))
         (define y (send event get-y))
         (set! text-snip-column (detect-column-by-x x))
         (set! text-snip-row (detect-row-by-y y))
         (when (and text-snip-column text-snip-row)
           (define column-height (send (send spreadsheet-editor get-row-button 0) get-height))
           (define column-width (send (send spreadsheet-editor get-column-button text-snip-column) get-width))
           (define column-x (send (send spreadsheet-editor get-column-button text-snip-column) get-x))
           (define column-y (send (send spreadsheet-editor get-row-button (- text-snip-row (get-field starting-row spreadsheet-editor))) get-y))
           (set! text-obj (new my-text% (pasteboard this)))
           (send text-obj insert ((get-field get-cell-contents spreadsheet-editor)
                                  ((get-field get-row spreadsheet-editor) text-snip-row)
                                  text-snip-column) 0)
           (send text-obj extend-position 0)
           (set! text-snip (new editor-snip%
                                (editor text-obj)
                                (left-margin 0) (right-margin 0) (top-margin 0) (bottom-margin 0)
                                (left-inset 0) (right-inset 0) (top-inset 0) (bottom-inset 0)
                                (with-border? #f)
                                (min-width column-width)
                                ;(max-width column-width)
                                (min-height column-height)
                                (max-height column-height)
                                ))
           (send this insert text-snip column-x column-y)
           (send this set-caret-owner text-snip)
           )))
      (void))))

(define spreadsheet-editor%
  (class table-panel%
    (super-new
     (alignment '(center center))
     (spacing 0)
     (stretchable-width #t)
     (stretchable-height #t)
     (horiz-margin 0)
     (vert-margin 0)
     (dimensions '(3 3)))
    
    (init-field n-rows)
    (init-field n-columns)
    (init-field (editable? #f))
    (init-field (horizontal-margin 4))
    (init-field (row-button-width 70))
    (init-field (get-column-label
                 (lambda (i)
                   (format "Col. ~a" i))))
    (init-field (get-row identity))
    (init-field (get-cell-contents
                 (lambda (row j)
                   (format "[~a, ~a]" row j)
                   )))
    (init-field (set-cell-contents!
                 (lambda (row col value)
                   (void))))
    (init-field (get-column-alignment
                 (lambda (j)
                   'right)))
    
    (init-field (column-button-left-click #f)
                (row-button-left-click #f))
    
    (field (starting-row 0)
           (starting-column 0))
    
    (define/public (get-n-rows) n-rows)
    
    (define/public (get-dc) (send editor-canvas get-dc))
    
    (define/public (get-visible-rows)
      (for/list
          ((row (in-range starting-row
                          (min n-rows (+ starting-row (get-field n-row-buttons vpanel-left))))))
        (get-row row)))
    
    (define/public (column-button-min-width i)
      (vector-ref column-buttons-min-widths i))
    
    (define/public (n-column-buttons)
      (get-field n-column-buttons hpanel-top))

    (define/public (n-row-buttons)
      (get-field n-row-buttons vpanel-left))
    
    (define/public (row-buttons)
      (get-field row-buttons vpanel-left))
    
    (define/public (visible-column-buttons)
      (send hpanel-top get-children))

    (define/public (text-snip-row)
      (get-field text-snip-row pasteboard))
    
    (define/public (text-snip-column)
      (get-field text-snip-column pasteboard))
    
    (define/public (get-column-button i)
      (vector-ref all-column-buttons i))
    
    (define/public (get-row-button i)
      (gvector-ref (get-field row-buttons vpanel-left) i))
    
    (define/public (set-button-label! idx label)
      (define button (vector-ref all-column-buttons idx))
      (send button set-label label)
      (vector-set! column-buttons-min-widths idx
                   (get-visible-text-width (send this get-dc) label))
      (send hpanel-top reflow-container)
      (send hpanel-top on-size
            (send hpanel-top get-width)
            (send hpanel-top get-height))
      (send editor-canvas refresh))
    
    (define/public (delete-row! i)
      (let ((r starting-row))
        (set! n-rows (- n-rows 1))
        (send vpanel-left reset-buttons)
        (for ((child (send vslider-pane get-children)))
          (send vslider-pane delete-child child))
        (for ((child (send vpanel-left get-children)))
          (send vpanel-left delete-child child))
        
        (send vpanel-left on-size
              (send vpanel-left get-width)
              (send vpanel-left get-height))
        (init-vslider)
        (send vslider set-value (max 1 r))
        (on-vslider-change vslider)
        ))
    
    (define/public (add-row-before! i)
      (let ((r starting-row))
        (set! n-rows (+ n-rows 1))
        (send vpanel-left reset-buttons)
        (for ((child (send vslider-pane get-children)))
          (send vslider-pane delete-child child))
        (for ((child (send vpanel-left get-children)))
          (send vpanel-left delete-child child))
        
        (send vpanel-left on-size
              (send vpanel-left get-width)
              (send vpanel-left get-height))
        (init-vslider)
        (send vslider set-value (+ r 1))
        (on-vslider-change vslider)
      ))
    
    (define/override (on-subwindow-char receiver event)
      (define text-snip (get-field text-snip pasteboard))
      (define n-row-buttons (send this n-row-buttons))
      (cond
        ((void? text-snip)
         (match-define (list delta-x delta-y)
           (match (send event get-key-code)
             ('left  '(-1  0))
             ('right '( 1  0))
             ((or 'up 'wheel-up)
              '( 0 -1))
             ((or 'down 'wheel-down)
              '( 0  1))
             ('prior `( 0 ,(- 1 n-row-buttons)))
             ('next  `( 0 ,(- n-row-buttons 1)))
             ('home `(0 ,(- 1 (send vslider get-value))))
             ('end  `(0 ,(- n-rows (send vslider get-value))))
             (else   '( 0  0))))
         (unless (void? hslider)
           (let ((new-value (min (max (+ (send hslider get-value) delta-x) 1) n-columns)))
             (unless (= new-value (send hslider get-value))
               (send hslider set-value new-value)
               (on-hslider-change hslider))))
         (unless (void? vslider)
           (let ((new-value (min (max (+ (send vslider get-value) delta-y) 1) n-rows)))
             (unless (= new-value (send vslider get-value))
               (send vslider set-value new-value)
               (on-vslider-change vslider)))))
        (else
         (super on-subwindow-char receiver event))))
    
    ;; Top-left corner
    (new pane%
         (parent this)
         (horiz-margin 0)
         (vert-margin 0)
         (stretchable-width #f)
         (stretchable-height #f))
    
    (define hpanel-top
      (new my-horizontal-panel%
           (parent this)
           (border 0)
           (stretchable-width #t)
           (stretchable-height #f)))
    
    ;; Top-right corner
    (new pane%
         (parent this)
         (horiz-margin 0)
         (vert-margin 0)
         (stretchable-width #f)
         (stretchable-height #f))
    
    
    (define vpanel-left
      (new my-vertical-panel%
           (parent this)
           (border 0)
           (stretchable-width #f)
           (stretchable-height #t)
           (min-width row-button-width)
           (button-left-click row-button-left-click)
           ))
    
    (define pasteboard (new my-pasteboard% (spreadsheet-editor this)))
    
    ;; Canvas (middle cell in the 3x3 layout)
    (field (editor-canvas
            (new my-editor-canvas%
                 (editor pasteboard)
                 (spreadsheet-editor this)
                 (vertical-inset 0)
                 (horizontal-inset 0)
                 (parent this)
                 (style '(no-border no-hscroll no-vscroll)))))
    
    ;; Stub for vertical slider
    (define vslider-pane
      (new vertical-pane%
           (parent this)
           (stretchable-width #f)
           (stretchable-height #t)))
    
    ;; Bottom left corner
    (new pane%
         (parent this)
         (stretchable-width #f)
         (stretchable-height #f)
         (horiz-margin 0)
         (vert-margin 0)
         (min-width row-button-width))

    ;; Stub for horizontal slider
    (define hslider-pane
      (new horizontal-pane%
           (parent this)
           (stretchable-width #t)
           (stretchable-height #f)))
  
    ;; Bottom right corner
    (new pane%
         (parent this)
         (horiz-margin 0)
         (vert-margin 0)
         (stretchable-width #f)
         (stretchable-height #f))
    
    (define (calculate-starting-row pivot height)
      (define button-height (send (gvector-ref (send this row-buttons) 0) get-height))
      (define nbuttons-visible (floor (/ height button-height)))
      (cond
        ((< (+ pivot nbuttons-visible) n-rows) pivot)
        ((>= nbuttons-visible n-rows) 0)
        (else (- n-rows nbuttons-visible))))
    
    (define (on-vslider-change self)
      (define new-starting-row
        (calculate-starting-row
         (- (send self get-value) 1)
         (send vpanel-left get-height)))
      (unless (equal? starting-row new-starting-row)
        (set! starting-row new-starting-row)
        (send hpanel-top on-size
              (send hpanel-top get-width)
              (send hpanel-top get-height))
        (send vpanel-left on-size
          (send vpanel-left get-width)
          (send vpanel-left get-height))
        (send editor-canvas refresh)))
    
    (define vslider (void))
    
    (define (init-vslider)
      (unless (zero? n-rows)
        (set!
         vslider
         (new slider%
              (parent vslider-pane)
              (stretchable-width #f)
              (stretchable-height #t)
              (label "")
              (style '(vertical plain))
              (min-value 1)
              (max-value n-rows)
              (callback
               (lambda (self event)
                 (on-vslider-change self)))))))
    
    (init-vslider)
    
    ;; Helper functions for scrolling
    (define (calculate-starting-column pivot width)
      (define n-buttons 0)
      (define column pivot)
      (define total-length 0)
      (let loop ((i pivot)
                 (length 0))
        (cond
          ((>= i (vector-length all-column-buttons))
           (let loop1 ((i pivot) (length length))
             (cond
               ((zero? i) 0)
               ((<= (+ length (send (vector-ref all-column-buttons (- i 1)) get-width)) width)
                (loop1 (- i 1) (+ length (send (vector-ref all-column-buttons (- i 1)) get-width))))
               (else i))))
          ((> length width) pivot)
          (else
           (loop (+ i 1) (+ length (send (vector-ref all-column-buttons i) get-width)))))))
    
    (define (on-hslider-change self)
      (define new-starting-column
        (calculate-starting-column
         (- (send self get-value) 1)
         (send hpanel-top get-width)))
      (unless (equal? starting-column new-starting-column)
        (set! starting-column new-starting-column)
        (send hpanel-top begin-container-sequence)
        (send hpanel-top change-children (lambda (x) '()))
        (send hpanel-top on-size
              (send hpanel-top get-width)
              (send hpanel-top get-height))
        (send hpanel-top end-container-sequence)
        (send editor-canvas refresh)))

    
    (define hslider
      (new slider%
           (parent hslider-pane)
           (label "")
           (style '(horizontal plain))
           (min-value 1)
           (max-value n-columns)
           (stretchable-width #t)
           (stretchable-height #f)
           (callback
            (lambda (self event)
              (on-hslider-change self)))))
    
    (field
     (all-column-buttons
      (for/vector
          ((i (in-range n-columns)))
        (new special-button%
             (parent hpanel-top)
             (stretchable-width #f)
             (min-width 80)
             (style '(deleted))
             (index i)
             (label ((get-field get-column-label this) i))
             (on-left-click
              (and column-button-left-click
                   (column-button-left-click this i))))
        )))
    
    (define column-buttons-min-widths
      (for/vector
          ((button all-column-buttons))
        (get-visible-text-width
         (send editor-canvas get-dc)
         (send button get-label))))
    ))
