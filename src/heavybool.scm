;; heavybool.scm - Reflective Boolean values with explanations

(use-modules (srfi srfi-9))  ; For define-record-type

;; Define the heavy-bool record type
(define-record-type <heavy-bool>
  (make-heavy-bool value because)
  heavy-bool?
  (value heavy-bool-value)
  (because heavy-bool-because))

;; Standard constants
(define heavy-true (make-heavy-bool #t '()))
(define heavy-false (make-heavy-bool #f '()))

;; Ensure a value is a heavy-boolean
(define (ensure-heavy-bool hb)
  (cond ((eq? hb #t) heavy-true)
        ((eq? hb #f) heavy-false)
        ((heavy-bool? hb) hb)
        (else (error "Not a valid heavy-bool" hb))))

;; Convert to standard boolean
(define (bool-value hb)
  (heavy-bool-value (ensure-heavy-bool hb)))

;; Logical negation
(define (not-m hb)
  (let ((hb (ensure-heavy-bool hb)))
    (make-heavy-bool (not (heavy-bool-value hb))
                    (heavy-bool-because hb))))

;; Add annotation to reason list
(define (annotate hb item)
  (let ((hb (ensure-heavy-bool hb)))
    (if (not (null? item))
        (make-heavy-bool (heavy-bool-value hb)
                        (cons item (heavy-bool-because hb)))
        hb)))

;; Only annotate if true
(define (annotate-true hb item)
  (let ((hb (ensure-heavy-bool hb)))
    (if (heavy-bool-value hb)
        (annotate hb item)
        hb)))

;; Only annotate if false
(define (annotate-false hb item)
  (let ((hb (ensure-heavy-bool hb)))
    (if (not (heavy-bool-value hb))
        (annotate hb item)
        hb)))

;; Find reason by key
(define (find-reason hb key)
  (unless (heavy-bool? hb)
    (error "find-reason expects a heavy-bool, got:" hb))
  (let ((reasons (heavy-bool-because hb)))
    (let loop ((reasons reasons))
      (cond ((null? reasons) #f)
            ((and (pair? (car reasons)) (assq key (car reasons))) => cdr)
            (else (loop (cdr reasons)))))))

;; Find witness value
(define (find-witness hb)
  (find-reason hb 'witness))

;; Logical operations
(define (and-m hb1 hb2)
  (let ((hb1 (ensure-heavy-bool hb1)))
    (if (heavy-bool-value hb1)
        hb2
        hb1)))

(define (or-m hb1 hb2)
  (let ((hb1 (ensure-heavy-bool hb1)))
    (if (heavy-bool-value hb1)
        hb1
        hb2)))

;; Display method for debugging
(define (display-heavy-bool hb)
  (let ((val (heavy-bool-value hb))
        (reasons (heavy-bool-because hb)))
    (if val
        (format #t "True~a" reasons)
        (format #t "False~a" reasons))))
