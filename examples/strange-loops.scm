;; strange-loops.scm - Hofstadter-style self-referential structures with HeavyBool
;; Demonstrates how HeavyBool's ability to track its own history creates strange loops

(load "../src/heavybool.scm")
(load "../src/heavybool-quantifiers.scm")
(load "../src/heavybool-monad.scm")

(display "===== STRANGE LOOPS WITH HEAVYBOOL =====\n")

;; 1. Self-referential reasoning
(display "\n1. SELF-REFERENTIAL REASONING\n")
(display "Creating a HeavyBool that reasons about its own creation...\n")

(define self-aware-true
  (let ((reason '((meta . "I think, therefore I am true")
                  (created-by . "strange-loops.scm")
                  (self-reference . #t))))
    (make-heavy-bool #t reason)))

(display "Self-aware HeavyBool: ")
(display-heavy-bool self-aware-true)
(newline)

;; 2. Recursive reasoning chains
(display "\n2. RECURSIVE REASONING CHAINS\n")
(display "Creating HeavyBools that reference each other...\n")

(define (create-circular-reasoning n)
  (if (<= n 0)
      (make-heavy-bool #t '((base-case . "circular reasoning ends here")))
      (let ((next (create-circular-reasoning (- n 1))))
        (annotate next `((level . ,n)
                        (depends-on . ,(- n 1))
                        (circular . "this depends on the next level"))))))

(define circular-hb (create-circular-reasoning 3))
(display "Circular reasoning HeavyBool: ")
(display-heavy-bool circular-hb)
(newline)

;; 3. Self-modifying predicates using HeavyBool
(display "\n3. SELF-MODIFYING PREDICATES\n")
(display "A predicate that modifies its own behavior based on history...\n")

(define call-count 0)
(define (self-modifying-predicate x)
  (set! call-count (+ call-count 1))
  (let ((result (> x call-count))
        (reason `((call-number . ,call-count)
                  (input . ,x)
                  (threshold . ,call-count)
                  (adaptive . "threshold increases with each call"))))
    (make-heavy-bool result reason)))

(display "Testing self-modifying predicate on (5 4 3 2 1):\n")
(let ((test-data '(5 4 3 2 1)))
  (for-each (lambda (x)
              (let ((result (self-modifying-predicate x)))
                (display (format #f "  Input ~a: " x))
                (display-heavy-bool result)
                (newline)))
            test-data))

;; 4. Tangled hierarchy - truth depends on falsity
(display "\n4. TANGLED HIERARCHY\n")
(display "Creating interdependent truth values...\n")

(define (create-tangled-pair)
  (let* ((false-part (make-heavy-bool #f '((depends-on . "true-part")
                                          (paradox . "I am false because the other is true"))))
         (true-part (make-heavy-bool #t `((depends-on . "false-part")
                                         (paradox . "I am true because the other is false")
                                         (counterpart . ,false-part)))))
    (list true-part false-part)))

(let ((tangled-pair (create-tangled-pair)))
  (display "True part: ")
  (display-heavy-bool (car tangled-pair))
  (newline)
  (display "False part: ")
  (display-heavy-bool (cadr tangled-pair))
  (newline))

;; 5. Meta-reasoning about quantifiers
(display "\n5. META-REASONING ABOUT QUANTIFIERS\n")
(display "A quantifier that reasons about its own existence...\n")

(define (meta-forall tag pred items)
  (let* ((start-reason `((meta-operation . "forall")
                        (tag . ,tag)
                        (item-count . ,(length items))
                        (self-aware . "I am a quantifier examining my own behavior")))
         (base-result (annotate heavy-true start-reason))
         (final-result (forall-m tag pred items)))
    (if (heavy-bool-value final-result)
        (annotate final-result '((meta-conclusion . "All items satisfied predicate, as I predicted")))
        (annotate final-result '((meta-conclusion . "Found counterexample, as is my purpose"))))))

(define meta-result
  (meta-forall 'x
               (lambda (x) 
                 (make-heavy-bool (even? x) 
                                 `((test . "checking if") (value . ,x) (property . "even"))))
               '(2 4 6 8)))

(display "Meta-quantifier result (all even): ")
(display-heavy-bool meta-result)
(newline)

(define meta-result-with-odd
  (meta-forall 'x
               (lambda (x) 
                 (make-heavy-bool (even? x) 
                                 `((test . "checking if") (value . ,x) (property . "even"))))
               '(2 4 5 8)))

(display "Meta-quantifier result (with odd): ")
(display-heavy-bool meta-result-with-odd)
(newline)

;; 6. Self-validating logic
(display "\n6. SELF-VALIDATING LOGIC\n")
(display "A logical system that validates its own consistency...\n")

(define (self-validating-and hb1 hb2)
  (let* ((result (and-m hb1 hb2))
         (validation-reason `((operation . "self-validating-and")
                             (inputs . ,(list (heavy-bool-value hb1) (heavy-bool-value hb2)))
                             (result . ,(heavy-bool-value result))
                             (validation . ,(and (boolean? (heavy-bool-value hb1))
                                                (boolean? (heavy-bool-value hb2))
                                                (boolean? (heavy-bool-value result))
                                                (eq? (heavy-bool-value result)
                                                     (and (heavy-bool-value hb1) (heavy-bool-value hb2)))))
                             (self-check . "Logic validated successfully"))))
    (annotate result validation-reason)))

(let ((validated-result (self-validating-and (make-heavy-bool #t '((source . "test1")))
                                             (make-heavy-bool #f '((source . "test2"))))))
  (display "Self-validating AND result: ")
  (display-heavy-bool validated-result)
  (newline))

(display "\n===== END OF STRANGE LOOPS DEMONSTRATION =====\n")