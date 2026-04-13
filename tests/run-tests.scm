;; run-tests.scm - Comprehensive test suite for HeavyBool
;; This implements proper unit tests with assertions and test reporting

(load "../src/heavybool.scm")
(load "../src/heavybool-quantifiers.scm")
(load "../src/heavybool-monad.scm")

;; Test framework
(define test-count 0)
(define passed-count 0)
(define failed-tests '())

(define (assert-equal expected actual test-name)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin
        (set! passed-count (+ passed-count 1))
        (display (format #f "✓ ~a\n" test-name)))
      (begin
        (set! failed-tests (cons test-name failed-tests))
        (display (format #f "✗ ~a\n  Expected: ~a\n  Actual: ~a\n" test-name expected actual)))))

(define (assert-true actual test-name)
  (assert-equal #t actual test-name))

(define (assert-false actual test-name)
  (assert-equal #f actual test-name))

;; Assert that a thunk raises an error
(define (assert-error thunk test-name)
  (set! test-count (+ test-count 1))
  (let ((error-raised #f))
    (catch #t
      (lambda () (thunk))
      (lambda (key . args) (set! error-raised #t)))
    (if error-raised
        (begin
          (set! passed-count (+ passed-count 1))
          (display (format #f "✓ ~a\n" test-name)))
        (begin
          (set! failed-tests (cons test-name failed-tests))
          (display (format #f "✗ ~a\n  Expected: error to be raised\n  Actual: no error\n" test-name))))))

(define (run-test-suite name thunk)
  (display (format #f "\n=== ~a ===\n" name))
  (thunk)
  (newline))

(define (print-test-summary)
  (display (format #f "\n=== TEST SUMMARY ===\n"))
  (display (format #f "Total tests: ~a\n" test-count))
  (display (format #f "Passed: ~a\n" passed-count))
  (display (format #f "Failed: ~a\n" (- test-count passed-count)))
  (if (not (null? failed-tests))
      (begin
        (display "Failed tests:\n")
        (for-each (lambda (test) (display (format #f "  - ~a\n" test))) (reverse failed-tests))))
  (if (= passed-count test-count)
      (display "🎉 All tests passed!\n")
      (display "❌ Some tests failed!\n")))

;; Tests for basic HeavyBool functionality
(run-test-suite "Basic HeavyBool Operations"
  (lambda ()
    ;; Test record creation and accessors
    (let ((hb (make-heavy-bool #t '((reason . "test")))))
      (assert-true (heavy-bool? hb) "make-heavy-bool creates heavy-bool")
      (assert-equal #t (heavy-bool-value hb) "heavy-bool-value returns correct value")
      (assert-equal '((reason . "test")) (heavy-bool-because hb) "heavy-bool-because returns correct reasons"))
    
    ;; Test constants
    (assert-true (heavy-bool-value heavy-true) "heavy-true has true value")
    (assert-equal '() (heavy-bool-because heavy-true) "heavy-true has empty reasons")
    (assert-false (heavy-bool-value heavy-false) "heavy-false has false value")
    
    ;; Test ensure-heavy-bool
    (assert-true (heavy-bool? (ensure-heavy-bool #t)) "ensure-heavy-bool converts #t")
    (assert-true (heavy-bool-value (ensure-heavy-bool #t)) "ensure-heavy-bool #t has true value")
    (assert-true (heavy-bool? (ensure-heavy-bool #f)) "ensure-heavy-bool converts #f")
    (assert-false (heavy-bool-value (ensure-heavy-bool #f)) "ensure-heavy-bool #f has false value")
    
    ;; Test bool-value
    (assert-true (bool-value heavy-true) "bool-value extracts from heavy-true")
    (assert-false (bool-value heavy-false) "bool-value extracts from heavy-false")
    (assert-true (bool-value #t) "bool-value works with plain #t")
    (assert-false (bool-value #f) "bool-value works with plain #f")))

;; Tests for logical operations
(run-test-suite "Logical Operations"
  (lambda ()
    ;; Test not-m
    (let ((not-true (not-m heavy-true))
          (not-false (not-m heavy-false)))
      (assert-false (heavy-bool-value not-true) "not-m of true is false")
      (assert-true (heavy-bool-value not-false) "not-m of false is true"))
    
    ;; Test and-m
    (assert-true (heavy-bool-value (and-m heavy-true heavy-true)) "true AND true = true")
    (assert-false (heavy-bool-value (and-m heavy-true heavy-false)) "true AND false = false")
    (assert-false (heavy-bool-value (and-m heavy-false heavy-true)) "false AND true = false")
    (assert-false (heavy-bool-value (and-m heavy-false heavy-false)) "false AND false = false")
    
    ;; Test or-m
    (assert-true (heavy-bool-value (or-m heavy-true heavy-true)) "true OR true = true")
    (assert-true (heavy-bool-value (or-m heavy-true heavy-false)) "true OR false = true")
    (assert-true (heavy-bool-value (or-m heavy-false heavy-true)) "false OR true = true")
    (assert-false (heavy-bool-value (or-m heavy-false heavy-false)) "false OR false = false")))

;; Tests for annotation functions
(run-test-suite "Annotation Functions"
  (lambda ()
    ;; Test annotate  
    (let ((annotated (annotate heavy-true '((reason . "added")))))
      (assert-true (heavy-bool-value annotated) "annotate preserves value")
      (assert-equal '(((reason . "added"))) (heavy-bool-because annotated) "annotate adds reason"))
    
    ;; Test annotate-true
    (let ((true-annotated (annotate-true heavy-true '((when-true . "yes"))))
          (false-annotated (annotate-true heavy-false '((when-true . "no")))))
      (assert-equal '(((when-true . "yes"))) (heavy-bool-because true-annotated) "annotate-true adds to true")
      (assert-equal '() (heavy-bool-because false-annotated) "annotate-true skips false"))
    
    ;; Test annotate-false  
    (let ((true-annotated (annotate-false heavy-true '((when-false . "no"))))
          (false-annotated (annotate-false heavy-false '((when-false . "yes")))))
      (assert-equal '() (heavy-bool-because true-annotated) "annotate-false skips true")
      (assert-equal '(((when-false . "yes"))) (heavy-bool-because false-annotated) "annotate-false adds to false"))))

;; Tests for quantifiers
(run-test-suite "Quantifier Operations"
  (lambda ()
    ;; Test forall-m with all true
    (let ((all-positive (forall-m 'x (lambda (x) (make-heavy-bool (> x 0) '())) '(1 2 3))))
      (assert-true (heavy-bool-value all-positive) "forall-m all positive numbers"))
    
    ;; Test forall-m with some false
    (let ((not-all-positive (forall-m 'x (lambda (x) (make-heavy-bool (> x 0) '())) '(1 -2 3))))
      (assert-false (heavy-bool-value not-all-positive) "forall-m not all positive"))
    
    ;; Test exists-m
    (let ((some-positive (exists-m 'x (lambda (x) (make-heavy-bool (> x 0) '())) '(-1 2 -3))))
      (assert-true (heavy-bool-value some-positive) "exists-m some positive"))
    
    (let ((none-positive (exists-m 'x (lambda (x) (make-heavy-bool (> x 0) '())) '(-1 -2 -3))))
      (assert-false (heavy-bool-value none-positive) "exists-m none positive"))
    
    ;; Test any-m and all-m helpers
    (assert-true (heavy-bool-value (any-m (list heavy-true heavy-false))) "any-m with some true")
    (assert-false (heavy-bool-value (any-m (list heavy-false heavy-false))) "any-m with all false")
    (assert-true (heavy-bool-value (all-m (list heavy-true heavy-true))) "all-m with all true")
    (assert-false (heavy-bool-value (all-m (list heavy-true heavy-false))) "all-m with some false")))

;; Tests for monadic operations
(run-test-suite "Monadic Operations"
  (lambda ()
    ;; Test return-bool
    (let ((returned (return-bool #t)))
      (assert-true (heavy-bool-value returned) "return-bool creates true HeavyBool")
      (assert-equal '() (heavy-bool-because returned) "return-bool has empty reasons"))
    
    ;; Test bind-bool
    (let* ((hb (make-heavy-bool #t '((original . "reason"))))
           (f (lambda (x) (make-heavy-bool (not x) '((operation . "negation")))))
           (result (bind-bool hb f)))
      (assert-false (heavy-bool-value result) "bind-bool applies function")
      (assert-equal '((original . "reason") (operation . "negation")) (heavy-bool-because result) "bind-bool accumulates reasons"))
    
    ;; Test fmap-bool
    (let* ((hb (make-heavy-bool #t '((test . "fmap"))))
           (result (fmap-bool not hb)))
      (assert-false (heavy-bool-value result) "fmap-bool applies function to value")
      (assert-equal '((test . "fmap")) (heavy-bool-because result) "fmap-bool preserves reasons"))
    
    ;; Test monadic laws
    ;; Left identity: return a >>= f ≡ f a
    (let* ((a #t)
           (f (lambda (x) (make-heavy-bool (not x) '((op . "not")))))
           (left (bind-bool (return-bool a) f))
           (right (f a)))
      (assert-equal (heavy-bool-value left) (heavy-bool-value right) "Monad left identity law"))
    
    ;; Right identity: m >>= return ≡ m  
    (let* ((m (make-heavy-bool #t '((test . "right-identity"))))
           (left (bind-bool m return-bool)))
      (assert-equal (heavy-bool-value m) (heavy-bool-value left) "Monad right identity law")
      (assert-equal (heavy-bool-because m) (heavy-bool-because left) "Monad right identity law (reasons)")))) 

;; Tests for error handling and edge cases
(run-test-suite "Error Handling and Edge Cases"
  (lambda ()
    ;; Test find-reason with empty reasons
    (assert-false (find-reason heavy-true 'nonexistent) "find-reason returns #f for missing key")
    
    ;; Test find-witness
    (let ((hb-with-witness (make-heavy-bool #f '(((witness . 42) (other . "data"))))))
      (assert-equal 42 (find-witness hb-with-witness) "find-witness extracts witness value"))
    
    ;; Test empty list quantifiers
    (assert-true (heavy-bool-value (forall-m 'x (lambda (x) (make-heavy-bool #f '())) '())) "forall-m on empty list is true")
    (assert-false (heavy-bool-value (exists-m 'x (lambda (x) (make-heavy-bool #t '())) '())) "exists-m on empty list is false")))

;; Integration tests
(run-test-suite "Integration Tests"
  (lambda ()
    ;; Test complex nested quantifiers
    (let ((result (forall-m 'x 
                           (lambda (x) 
                             (exists-m 'y 
                                      (lambda (y) (make-heavy-bool (= (+ x y) 5) '()))
                                      '(1 2 3 4 5)))
                           '(1 2 3))))
      (assert-true (heavy-bool-value result) "Complex nested quantifiers work"))
    
    ;; Test monadic composition
    (let* ((f (lambda (x) (make-heavy-bool (not x) '((step . 1)))))
           (g (lambda (x) (make-heavy-bool (not x) '((step . 2)))))
           (composed (kleisli-compose g f))
           (result (composed #t)))
      (assert-true (heavy-bool-value result) "Kleisli composition works")
      (assert-equal '((step . 1) (step . 2)) (heavy-bool-because result) "Composition accumulates reasons"))))

;; Negative contract tests - verify error handling
(run-test-suite "Negative Contracts (Error Handling)"
  (lambda ()
    ;; ensure-heavy-bool must reject invalid input
    (assert-error (lambda () (ensure-heavy-bool 42))
                  "ensure-heavy-bool rejects integer")
    (assert-error (lambda () (ensure-heavy-bool "string"))
                  "ensure-heavy-bool rejects string")
    (assert-error (lambda () (ensure-heavy-bool '(1 2 3)))
                  "ensure-heavy-bool rejects list")

    ;; forall-m must reject non-procedure predicate
    (assert-error (lambda () (forall-m 'x 42 '(1 2 3)))
                  "forall-m rejects non-procedure predicate")
    (assert-error (lambda () (forall-m 'x "not-a-function" '(1 2 3)))
                  "forall-m rejects string predicate")

    ;; forall-m must reject non-list items
    (assert-error (lambda () (forall-m 'x (lambda (x) heavy-true) 42))
                  "forall-m rejects non-list items (integer)")
    (assert-error (lambda () (forall-m 'x (lambda (x) heavy-true) "string"))
                  "forall-m rejects non-list items (string)")

    ;; any-m must reject non-list input
    (assert-error (lambda () (any-m 42))
                  "any-m rejects non-list (integer)")
    (assert-error (lambda () (any-m "string"))
                  "any-m rejects non-list (string)")

    ;; all-m must reject non-list input
    (assert-error (lambda () (all-m 42))
                  "all-m rejects non-list (integer)")
    (assert-error (lambda () (all-m "string"))
                  "all-m rejects non-list (string)")

    ;; find-reason must reject non-heavy-bool input
    (assert-error (lambda () (find-reason 42 'key))
                  "find-reason rejects integer")
    (assert-error (lambda () (find-reason #t 'key))
                  "find-reason rejects plain boolean")
    (assert-error (lambda () (find-reason '((a . 1)) 'key))
                  "find-reason rejects alist")))

;; Monad law: Associativity
(run-test-suite "Monad Associativity Law"
  (lambda ()
    ;; Associativity: (m >>= f) >>= g ≡ m >>= (λx → f x >>= g)
    (let* ((m (make-heavy-bool #t '((start . "m"))))
           (f (lambda (x) (make-heavy-bool (not x) '((step . "f")))))
           (g (lambda (x) (make-heavy-bool x '((step . "g")))))
           ;; Left side: (m >>= f) >>= g
           (left (bind-bool (bind-bool m f) g))
           ;; Right side: m >>= (λx → f x >>= g)
           (right (bind-bool m (lambda (x) (bind-bool (f x) g)))))
      (assert-equal (heavy-bool-value left) (heavy-bool-value right)
                    "Monad associativity law (value)")
      (assert-equal (heavy-bool-because left) (heavy-bool-because right)
                    "Monad associativity law (reasons)"))))

;; Functor laws
(run-test-suite "Functor Laws"
  (lambda ()
    ;; Functor identity law: fmap id = id
    (let* ((hb (make-heavy-bool #t '((test . "functor-id"))))
           (id (lambda (x) x))
           (result (fmap-bool id hb)))
      (assert-equal (heavy-bool-value hb) (heavy-bool-value result)
                    "Functor identity law (value)")
      (assert-equal (heavy-bool-because hb) (heavy-bool-because result)
                    "Functor identity law (reasons)"))

    ;; Functor composition law: fmap (f . g) = fmap f . fmap g
    (let* ((hb (make-heavy-bool #t '((test . "functor-compose"))))
           (f not)
           (g (lambda (x) x))  ; identity, so f . g = not
           (compose (lambda (f g) (lambda (x) (f (g x)))))
           ;; Left side: fmap (f . g) hb
           (left (fmap-bool (compose f g) hb))
           ;; Right side: fmap f (fmap g hb)
           (right (fmap-bool f (fmap-bool g hb))))
      (assert-equal (heavy-bool-value left) (heavy-bool-value right)
                    "Functor composition law (value)")
      (assert-equal (heavy-bool-because left) (heavy-bool-because right)
                    "Functor composition law (reasons)"))))

;; Run all tests
(display "🧪 RUNNING HEAVYBOOL TEST SUITE 🧪\n")
(print-test-summary)