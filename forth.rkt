#lang racket

;; TODO: Add comments
;; TODO: Add strings
;; TODO: Stdlib coverage
;; TODO: Imperative shit (do loop, variables, arrays)!

;; err : string -> err
(define (err msg)
  (list 'err msg))

;; err? : ? -> bool
(define (err? e)
  (if (pair? e)
      (if (equal? (car e) 'err)
          #t
          #f)
      #f))

;; handle : err -> type -> val_of(type) | ?a -> type -> ?a
(define (handle e type)
  (if (err? e)
      (both
       (displayln (cadr e)) ; Display the error message
       (cond
        [(equal? type 'number) 0]
        [(equal? type 'proc) (lambda (x) x)]
        [(equal? type 'list) '()]
        [(equal? type 'string) ""]))
      e)) ; Return the value given, since it's okay

;; starts-with? : string -> char -> bool
(define (starts-with? str char)
  (char=? (car (string->list str)) char))

;; flush-and-return : term list -> term list
(define (flush-and-return terms)
  (read-char)
  terms)

;; read-terms : term list -> term list
(define (read-terms terms)
  (if (char=? (peek-char) #\newline)
      (flush-and-return terms)
      (read-terms (append terms (list (read-term '()))))))

;; read-term : char -> term
(define (read-term acc)
  (if (char=? (peek-char) #\newline)
      (list->string acc)
      (let ([next-char (read-char)])
        (if (char=? next-char #\space)
            (list->string acc)
            (read-term (append acc (list next-char)))))))

;; parse-term : term => ?
(define (parse-term term)
  (let ([maybe-num (string->number term)])
    (if maybe-num
        (push maybe-num)
        (if (starts-with? term #\')
            (push (substring term 1))
            (apply-func term)))))

;; contains? : ? list -> ? -> bool
(define (contains? items item)
  (if (null? items)
      #f
      (if (equal? (car items) item)
          #t
          (contains? (cdr items) item))))

;; split : ? list -> ? list -> ? -> ? list * ? list
(define (split acc items target)
  (if (null? items)
      (list acc items)
      (if (equal? target (car items))
          (list acc (cdr items))
          (split (append acc (list (car items))) (cdr items) target))))

;; parse-def : {def a b} term -> void
(define (parse-def terms)
  (let* ([name (car terms)]
         [rest (split '() (cdr terms) ";")]
         [fn (lambda ()
               (eval-terms (car rest)))])
    (eval-terms (cadr rest))
    (new-func name fn)))

;; parse-if : {if a (?b)} term -> a | (b | void)
(define (parse-if terms)
  (let* ([clauses (split '() terms "then")]
         [subclauses (split '() (car clauses) "else")])
    (if (contains? (car clauses) "else")
        (if (zero? (peek))
            (both (pop) (eval-terms (car subclauses)))
            (if (equal? (pop) -1)
                (eval-terms (cadr subclauses))
                #f))
        (if (equal? (pop) -1)
            (eval-terms (car clauses))
            #f))))

;; string * (? -> ?) list
(define funcs '())

;; string -> (? -> ?) => string * (? -> ?) list
(define (new-func name lam)
  (set! funcs (cons (list name lam) funcs)))

;; string * (? -> ?) list => ?
(define (apply-func name)
  (let ([fns (filter
              (lambda (func)
                (string=? name (car func)))
              funcs)])
    (if (null? fns)
        (err (string-append name " ?"))
        (apply (cadr (car fns)) '()))))

;; ? stack
(define stack '())

;; ? => ? stack
(define (push x)
  (set! stack (cons x stack)))

;; ? stack -> ?
(define (peek)
  (if (null? stack)
      (err "Underflow error!")
      (car stack)))

;; ? stack => ?
(define (pop)
  (if (null? stack)
      (err "Underflow error!")
      (let ([item (car stack)])
        (set! stack (cdr stack))
        item)))

;; both : ?a -> ?b -> ?b
(define (both a b)
  b)

;; eval-terms : term list => ?
(define (eval-terms terms)
  (if (null? terms)
      '()
      (if (string=? (car terms) ":")
          (parse-def (cdr terms))
          (if (string=? (car terms) "if")
              (parse-if (cdr terms))
              (both
               (parse-term (car terms))
               (eval-terms (cdr terms)))))))

;; print-stack : stack => string out
(define (print-stack)
  (displayln (reverse stack)))

(new-func "+"
          (lambda ()
            (push (+ (handle (pop) 'number) (handle (pop) 'number)))))

(new-func "-"
          (lambda ()
            (let ([first (pop)])
              (push (- (handle (pop) 'number) (handle first 'number))))))

(new-func "*"
          (lambda ()
            (push (* (handle (pop) 'number) (handle (pop) 'number)))))

(new-func "/"
          (lambda ()
            (let ([first (pop)])
              (push (/ (handle (pop) 'number) (handle first 'number))))))

(new-func "="
          (lambda ()
            (if (equal? (pop) (pop))
                (push -1)
                (push 0))))

(new-func "<"
          (lambda ()
            (let ([first (pop)])
              (if (< (pop) first)
                  (push -1)
                  (push 0)))))

(new-func ">"
          (lambda ()
            (let ([first (pop)])
              (if (> (handle (pop) 'number) (handle first 'number))
                  (push -1)
                  (push 0)))))

(new-func "."
          (lambda ()
            (display (handle (pop) 'string)))) ; "" will display as empty

(new-func "emit"
          (lambda ()
            (display (integer->char (handle (pop) 'number)))))

(new-func "cr"
          (lambda ()
            (displayln "")))

(new-func "dup"
          (lambda ()
            (push (handle (peek) 'number))))

(new-func "drop"
          (lambda ()
            (pop)))

(new-func "swap"
          (lambda ()
            (let ([first (pop)]
                  [second (pop)])
              (cond
               [(and (err? first) (err? second)) (handle (err "Underflow error!") 'string)]
               [(err? second) (push first) (handle (err "Underflow error!") 'string)]
               [else
                (push first)
                (push second)]))))

(new-func "over"
          (lambda ()
            (let ([first (pop)]
                  [second (pop)])
              (cond
               [(and (err? first) (err? second)) (handle (err "Underflow error!") 'string)]
               [(err? second) (push first) (handle (err "Underflow error!") 'string)]
               [else
                (push second)
                (push first)
                (push second)]))))

(new-func "rot"
          (lambda ()
            (let ([first (pop)]
                  [second (pop)]
                  [third (pop)])
              (cond
               [(and (err? first) (err? second) (err? third)) (handle (err "Underflow error!") 'string)]
               [(and (err? second) (err? third)) (push first) (handle (err "Underflow error!") 'string)] ; We are `rot`ing here still... is this a good idea?
               [(err? third) (push first) (push second) (handle (err "Underflow error!") 'string)]
               [else
                (push second)
                (push first)
                (push third)]))))

(new-func "empty?"
          (lambda ()
            (if (null? stack)
                (push -1)
                (push 0))))

(new-func "one?"
          (lambda ()
            (if (null? stack)
                (push 0)
                (if (null? (cdr stack))
                    (push -1)
                    (push 0)))))

;; Definitions that are possible within Forth:
(eval-terms (list ":" "not" "if" "-1" "else" "0" "then" ";"))
(eval-terms (list ":" "or" "if" "else" "drop" "-1" "then" ";"))
(eval-terms (list ":" "and" "if" "drop" "0" "else" "then" ";"))
(eval-terms (list ":" "xor" "not" "if" "not" "else" "then" ";")) ; Can be changed to "not ="
;; TODO: >=, <=, etc. functions
(eval-terms (list ":" "<>" "xor" ";"))
(eval-terms (list ":" "sep" "'|" ";"))
(eval-terms (list ":" "sep?" "dup" "sep" "=" ";"))
(eval-terms (list ":" "print" "dup" "." ";"))
(eval-terms (list ":" "print*" "dup" "sep?" "swap" "drop" "if" "." "print*" "else" "drop" "then" ";"))
(eval-terms (list ":" "drop*" "dup" "sep?" "swap" "drop" "if" "drop" "drop*" "else" "drop" "then" ";"))
(eval-terms (list ":" "sum" "one?" "if" "sumh" "else" "then" ";"))
(eval-terms (list ":" "sumh" "empty?" "swap" "sep?" "rot" "or" "not" "if" "+" "sum" "then" ";"))

;; Main function
(define (loop)
  (display "[$] ")
  (flush-output)
  (eval-terms (read-terms '()))
  (print-stack)
  (loop))

(loop)
