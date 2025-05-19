;; associativity-test.scm - Testing algebraic properties with HeavyBool
;; Based on Jim Newton's ELS'25 presentation

;; Fix the path issues by using proper relative paths
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
    (let* ((a-reason (find-reason result 'witness))
           (a (if a-reason a-reason 'unknown))
           (b-reason (find-reason (car (heavy-bool-because result)) 'witness))
           (b (if b-reason b-reason 'unknown))
           (c-reason (find-reason (cadr (heavy-bool-because result)) 'witness))
           (c (if c-reason c-reason 'unknown)))
      (display "Counterexample found: ")
      (display (list a b c))
      (newline)
      (let ((left (if (and (number? a) (number? b) (number? c))
                      (- (- a b) c)
                      'unknown))
            (right (if (and (number? a) (number? b) (number? c))
                       (- a (- b c))
                       'unknown)))
        (display (format #f "  (a - b) - c = (~a - ~a) - ~a = ~a\n" 
                        a b c left))
        (display (format #f "  a - (b - c) = ~a - (~a - ~a) = ~a\n" 
                        a b c right))))))
