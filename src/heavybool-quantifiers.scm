;; heavybool-quantifiers.scm - Quantifier operations for HeavyBool

;; Note: heavybool.scm should be loaded before this file

;; Universal quantifier implementation
(define (forall-m tag pred items)
  (unless (procedure? pred)
    (error "forall-m expects a predicate function, got:" pred))
  (unless (list? items)
    (error "forall-m expects a list of items, got:" items))
  (let loop ((items items)
             (result heavy-true))
    (if (null? items)
        result
        (let ((this-result (ensure-heavy-bool (pred (car items)))))
          (if (heavy-bool-value this-result)
              (loop (cdr items) result)
              (annotate this-result `((witness . ,(car items)) 
                                      (var . ,tag))))))))

;; Existential quantifier implementation
(define (exists-m tag pred items)
  (not-m (forall-m tag 
                  (lambda (x) (not-m (ensure-heavy-bool (pred x))))
                  items)))

;; Helper for any-m - returns first truthy value
(define (any-m hbools)
  (unless (list? hbools)
    (error "any-m expects a list of HeavyBools, got:" hbools))
  (let loop ((hbs hbools))
    (cond ((null? hbs) heavy-false)
          ((heavy-bool-value (ensure-heavy-bool (car hbs))) (car hbs))
          (else (loop (cdr hbs))))))

;; Helper for all-m - returns first falsey value or true
(define (all-m hbools)
  (unless (list? hbools)
    (error "all-m expects a list of HeavyBools, got:" hbools))
  (let loop ((hbs hbools))
    (cond ((null? hbs) heavy-true)
          ((not (heavy-bool-value (ensure-heavy-bool (car hbs)))) (car hbs))
          (else (loop (cdr hbs))))))
