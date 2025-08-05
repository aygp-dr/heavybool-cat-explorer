;; heavybool-monad.scm - Monadic operations for HeavyBool
;; This implements HeavyBool as an instance of the Writer monad

;; Note: heavybool.scm should be loaded before this file

;; Monadic return (unit) - wraps a boolean value in HeavyBool with empty reason list
(define (return-bool value)
  (make-heavy-bool value '()))

;; Monadic bind (flatmap) - chains HeavyBool computations
;; Takes a HeavyBool and a function that takes a boolean and returns a HeavyBool
(define (bind-bool hb f)
  (let* ((hb-unwrapped (ensure-heavy-bool hb))
         (value (heavy-bool-value hb-unwrapped))
         (reasons (heavy-bool-because hb-unwrapped))
         (result (f value)))
    (let ((result-unwrapped (ensure-heavy-bool result)))
      (make-heavy-bool (heavy-bool-value result-unwrapped)
                      (append reasons (heavy-bool-because result-unwrapped))))))

;; Kleisli composition operator
;; Composes two functions that return HeavyBools
(define (kleisli-compose g f)
  (lambda (x)
    (bind-bool (f x) g)))

;; Functor map operation
;; Maps a function over the boolean value inside a HeavyBool
(define (fmap-bool f hb)
  (let ((hb-unwrapped (ensure-heavy-bool hb)))
    (make-heavy-bool (f (heavy-bool-value hb-unwrapped))
                    (heavy-bool-because hb-unwrapped))))

;; Applicative pure (same as return for monads)
(define pure-bool return-bool)

;; Applicative apply - applies a function wrapped in HeavyBool to a value in HeavyBool
(define (apply-bool hf hv)
  (bind-bool hf
             (lambda (f)
               (bind-bool hv
                          (lambda (v)
                            (return-bool (f v)))))))

;; Utility: lift a regular function to work with HeavyBools
(define (lift-bool f)
  (lambda (hb)
    (fmap-bool f hb)))

;; Utility: lift a binary function to work with HeavyBools
(define (lift2-bool f)
  (lambda (hb1 hb2)
    (bind-bool hb1
               (lambda (v1)
                 (bind-bool hb2
                            (lambda (v2)
                              (return-bool (f v1 v2))))))))

;; Monadic versions of logical operations (already defined in heavybool.scm but with cleaner implementation)
(define (and-bool-m hb1 hb2)
  ((lift2-bool (lambda (x y) (and x y))) hb1 hb2))

(define (or-bool-m hb1 hb2)
  ((lift2-bool (lambda (x y) (or x y))) hb1 hb2))

(define (not-bool-m hb)
  ((lift-bool not) hb))

;; Tell operation (for Writer monad) - adds a reason without changing the value
(define (tell-bool hb reason)
  (let ((hb-unwrapped (ensure-heavy-bool hb)))
    (make-heavy-bool (heavy-bool-value hb-unwrapped)
                    (cons reason (heavy-bool-because hb-unwrapped)))))

;; Listen operation (for Writer monad) - returns both the value and the accumulated reasons
(define (listen-bool hb)
  (let ((hb-unwrapped (ensure-heavy-bool hb)))
    (cons (heavy-bool-value hb-unwrapped)
          (heavy-bool-because hb-unwrapped))))

;; Pass operation (for Writer monad) - applies a function to the reasons
(define (pass-bool hb f)
  (let ((hb-unwrapped (ensure-heavy-bool hb)))
    (make-heavy-bool (heavy-bool-value hb-unwrapped)
                    (f (heavy-bool-because hb-unwrapped)))))

;; Censor operation - filters or transforms the accumulated reasons
(define (censor-bool hb f)
  (pass-bool hb f))

;; Monadic sequence operation - sequences a list of HeavyBool computations
(define (sequence-bool hb-list)
  (if (null? hb-list)
      (return-bool '())
      (bind-bool (car hb-list)
                 (lambda (x)
                   (bind-bool (sequence-bool (cdr hb-list))
                              (lambda (xs)
                                (return-bool (cons x xs))))))))

;; Monadic mapM operation - maps a monadic function over a list
(define (mapM-bool f lst)
  (sequence-bool (map f lst)))