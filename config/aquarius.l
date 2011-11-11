;; -*- scheme -*-

(define buffer-list '())

(define (enable-debug . list)
  (define (ed xs) 
    (if (null? xs) '()
	(begin (aquarius-debug (car xs))
	       (ed (cdr xs)))))
  (ed list))

(define (get-binding item name)
  (cond ((language-grammar? item) (get-binding-from-grammar item name))
	((syntax-tree? item) (get-binding-from-syntax-tree item name))
	((program-tree? item) (get-binding-from-program-tree item name))
	(else (error "attempt to get a binding from " name))
	)
  )

(define (get-grammar-binding name)
  (get-binding current-grammar name))

(define (set-format item before after)
  (let ((name-binding (get-binding current-grammar item))
	(fmt (make-format before after)))
	(cond 
	 ((null? name-binding) (error "no such symbol" item))
	 ((symbol? name-binding)
	  (format-symbol name-binding fmt))
	 ((syntax-tree? name-binding)
	  (format-syntax-tree name-binding fmt))
	 (else (error "invalid target for format: " name-binding))))
  )


(define (child-path start list)
  (if (null? list) start
      (child-path (get-binding start (car list)) (cdr list)))
  )

(define (format-child tree child before after)
  (format-syntax-tree (child-path tree (list child)) 
		      (make-format before after))
  )

(define (format-child-n tree child n before after)
  (format-syntax-tree (syntax-find-child-n tree child n)
		      (make-format before after)))

; (define (property item list)
;   (if (= list '()) 
;       '()
;     (begin (add-property item (car list)) (property item (cdr list)))))

; (define (add-property item prop)
;   (if (= prop 'symbol-table)
;       (lib-add-symbol-table item)
;     (lib-add-property item prop)))

(define (align-child item path)
  (lib-align-child item (child-path item path)))

(define current-grammar #f)

(define (load-grammar name syn-file rules-file)
  (set! current-grammar (new-grammar name))
  (create-grammar-frame current-grammar)
  (parse-ebnf-file syn-file current-grammar)
  (parse-rule-file rules-file current-grammar))

(define (local-add-semantics tree time action)
  (add-semantics tree time action))

;(define-macro (sem tree time action)
;  `(local-add-semantics ,tree ,time ,(lambda (current-tree) action)))

(define-macro (sem tree time action)
  `(add-semantic-action current-grammar ,tree 'default ,time 
		  (lambda (current-buffer current-tree) ,action)))

(define-macro (sem-phase tree phase time action)
  `(add-semantic-action current-grammar ,tree ,phase ,time
                  (lambda (current-buffer current-tree) ,action)))

(define (sem-choice tree choice-matches)
  (cond ((null? choice-matches) (error "no choice match"))
	((not (empty-program-tree? (get-child tree (caar choice-matches))))
	 (primitive-eval (cadar choice-matches)))
	(#t (sem-choice tree (cdr choice-matches)))))

(define-macro (default-rule name action)
  `(create-default-rule current-grammar ,name
			(lambda (current-tree) ,action)))

(define-macro (rule name syn-tree action)
  `(create-rule ,syn-tree ,name (lambda (current-tree) ,action)))

(define-macro (semantic-error tree message)
  `(report-error current-buffer ,tree ,message))

(define-macro (code tree time result)
  `(add-code ,tree ,time 
		  (lambda (current-buffer current-tree) ,result)))
  
(define (new-table tree name . lparent)
  (let ((tbl (if (eq? lparent '())
		 (new-symbol-table)
		 (new-symbol-table-with-parent (car lparent)))))
    (add-symbol-table tree name tbl)))

(define (for-each-child tree name action)
  (let* ((child-syntax (syntax-find-child (get-syntax tree) name))
	 (children (find-children-with-syntax tree child-syntax)))
    (map action children)))

(define (syntax-has-child tree name)
  (let ((child-syntax (syntax-find-child (get-syntax tree) name)))
    (not (syntax-empty? child-syntax))))

(define (get-child tree name)
  (let ((child-syntax (syntax-find-child (get-syntax tree) name)))
    (find-child-with-syntax tree child-syntax)))

(define (has-child tree name)
  (not (empty-program-tree? (get-child tree name))))

(define (get-child-n tree name index)
  (let ((child-syntax (syntax-find-child-n (get-syntax tree) name index)))
    (find-child-with-syntax tree child-syntax)))

(define (table-entry-add-property-list ent props)
  (if (null? props) '()
      (begin (table-entry-add-property (caar props) (cadar props))
	     (table-entry-add-property-list ent (cdr props)))))

(define (table-entry-add-property ent prop value)
  (set-property ent prop value))

(define (add-entry tbl name tag defn . props)
  (let ((ent (new-table-entry name tag)))
    (table-entry-add-property ent 'defn defn)
    (map (lambda (x) (table-entry-add-property ent (car x) (cadr x))) 
	 props)
;    (table-entry-add-property-list ent props)
    (table-insert tbl ent)))

(define (new-unique-name current-buffer name tag tree . props)
  (let ((tbl (get-symbol-table tree)))
    (if (table-entry-found? (table-get-with-tag tbl name tag #F))
	(report-error current-buffer tree 
		      (string-append "redefined "
				     (symbol->string tag)
				     ": "
				     (symbol->string name)))
	(let ((ent (new-table-entry name tag)))
	  (table-entry-add-property ent 'defn tree)
	  (map (lambda (x) (table-entry-add-property ent (car x) (cadr x))) 
	       props)
	  (table-insert tbl ent)
	  ent))))
  
(define (get-properties item)
  (cond ((program-tree? item) (tree-get-properties item))
	((table-entry? item) (table-entry-properties item))
	(else (error "invalid type for get-properties"))))


(define (has-property item name)
  (prop-has-property? (get-properties item) name))

(define (get-property item name)
  (prop-get-property (get-properties item) name))

(define (set-property item name value)
  (prop-set-property (get-properties item) name value))

(define (remove-property item name)
  (prop-remove-property (get-properties item) name))


(define (ada-case-identifier x)
  (define (case-id-list list first)
    (cond 
     ((null? list) '())
     ((eq? (car list) #\_) (cons (car list) (case-id-list (cdr list) #t)))
     ((and first (char-lower-case? (car list)))
      (cons (char-upcase (car list)) (case-id-list (cdr list) #f)))
     ((and (char-upper-case? (car list)) (not first))
      (cons (char-downcase (car list)) (case-id-list (cdr list) #f)))
     (else (cons (car list) (case-id-list (cdr list) #f)))))

  (let* ((xlist (string->list x)))
    (list->string (case-id-list xlist #t))))

(define (copy-children current-tree current-grammar rule-name)
  (define (cc child)
    (if (empty-program-tree? child) 
	'()
	(cons (copy-tree child current-grammar rule-name)
	      (cc (get-right-sibling child)))))
  (cc (get-first-child current-tree)))


;(define (create-hook name hook)
;  (add-hook (get-grammar-binding name) 'create hook))
;

;--------------------------------
; Grammar bindings
;--------------------------------

; (use-modules (guile-lang-allover rx))
; (define grammar-binding-list '())
; (define (add-grammar-binding pattern grammar-name)
;   (set! grammar-binding-list
; 	(cons (list (regcomp pattern) grammar-name) grammar-binding-list))
;   )
; (define (get-grammar-name buffer-name)
;   (define (ggn bindings)
;     (cond ((null? bindings) '())
; 	  ((regexec (caar bindings) buffer-name) (cadar bindings))
; 	  (else (ggn (cdr bindings)))
; 	  )
;     )
;   (ggn grammar-binding-list)
;   )

; (add-grammar-binding "*.ad[sba]" 'ada)
