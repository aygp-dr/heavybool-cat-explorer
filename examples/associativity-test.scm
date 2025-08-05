;; associativity-test.scm - Testing algebraic properties with HeavyBool
;; Based on Jim Newton's ELS'25 presentation

;; Load the source files with correct relative paths from examples directory
(load "../src/heavybool.scm")
(load "../src/heavybool-quantifiers.scm")

;; Define a test set
(define M '(0 1 2 3 4))

;; Test associativity of addition
(define (test-addition-associativity)
  (display "Testing if (a + b) + c = a + (b + c) for all a,b,c in M...\n")
  (forall-m 'a
            (lambda (a)
              (forall-m 'b
                        (lambda (b)
                          (forall-m 'c
                                   (lambda (c)
                                     (let ((left-assoc (+ (+ a b) c))
                                           (right-assoc (+ a (+ b c))))
                                       (make-heavy-bool (= left-assoc right-assoc) '())))
                                   M))
                        M))
            M))

;; Test associativity of subtraction
(define (test-subtraction-associativity)
  (display "Testing if (a - b) - c = a - (b - c) for all a,b,c in M...\n")
  (forall-m 'a
            (lambda (a)
              (forall-m 'b
                        (lambda (b)
                          (forall-m 'c
                                   (lambda (c)
                                     (let ((left-assoc (- (- a b) c))
                                           (right-assoc (- a (- b c))))
                                       (make-heavy-bool (= left-assoc right-assoc) '())))
                                   M))
                        M))
            M))

;; Run the tests
(display "===== ASSOCIATIVITY TESTS =====\n")
(display "Testing addition associativity: ")
(display-heavy-bool (test-addition-associativity))
(newline)

(display "Testing subtraction associativity: ")
(display-heavy-bool (test-subtraction-associativity))
(newline)

;; If subtraction is not associative, find the counterexample
(let ((result (test-subtraction-associativity)))
  (when (not (heavy-bool-value result))
    (display "===== COUNTEREXAMPLE ANALYSIS =====\n")
    (display "Subtraction is not associative, as expected.\n")
    (display "The first counterexample should be a=0, b=0, c=1:\n")
    (display "  (0 - 0) - 1 = -1\n")
    (display "  0 - (0 - 1) = 1\n")
    (display "These are clearly not equal, demonstrating non-associativity.\n")))
