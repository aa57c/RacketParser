#lang racket
(require 2htdp/batch-io)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

; Lexer: Scanner for input files
(define calc-lexer
  (lexer
   ["write" (cons `WRITE (calc-lexer input-port))]  ; Match the keyword "write"
   ["read" (cons `READ (calc-lexer input-port))]    ; Match the keyword "read"
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))  ; Match identifiers (letters a-z, A-Z)
    (cons `ID (calc-lexer input-port))]
   [(:: (:? #\:) (:+ #\=)) (cons `ASS_OP (calc-lexer input-port))]  ; Match assignment operator ":="
   [(:or #\* #\/) (cons `MULT_OP (calc-lexer input-port))]  ; Match multiplication operator "*" or "/"
   [(:+ (:or #\$)) (cons `END (calc-lexer input-port))]  ; Match the end of the input "$"
   [#\( (cons `LPAR (calc-lexer input-port))]  ; Match opening parenthesis "("
   [#\) (cons `RPAR (calc-lexer input-port))]  ; Match closing parenthesis ")"
   [(:: (:? #\-) (:+ (char-range #\0 #\9))) (cons `INT (calc-lexer input-port))]  ; Match integers
   [(:or #\+ #\-) (cons `ADD_OP (calc-lexer input-port))]  ; Match addition operator "+" or "-"
   [whitespace (calc-lexer input-port)]  ; Ignore whitespace
   [(eof) '()]  ; End of file (EOF) condition
))

; Match function for token validation
(define (match expected token-list)
  (cond
    [(null? token-list) (error 'match (format "Unexpected end of tokens, expected: ~a" expected))]  ; If list is empty, throw an error
    [(equal? expected (first token-list)) (rest token-list)]  ; If the expected token matches, return the rest of the tokens
    [else (error 'match (format "Syntax error: expected ~a but found ~a" expected (first token-list)))]))  ; Syntax error if unexpected token is found

; Addition operator grammar
(define (add-op token-list)
  (cond
    [(equal? (first token-list) 'ADD_OP) (match 'ADD_OP token-list)]  ; Match addition operator
    [else (error 'add-op "Syntax error in addition operator")]))  ; Error if not an addition operator

; Multiplication operator grammar
(define (mult-op token-list)
  (cond
    [(equal? (first token-list) 'MULT_OP) (match 'MULT_OP token-list)]  ; Match multiplication operator
    [else (error 'mult-op "Syntax error in multiplication operator")]))  ; Error if not a multiplication operator

; Factor grammar
(define (factor token-list)
  (cond
    [(equal? (first token-list) 'ID) (match 'ID token-list)]  ; Match an identifier (ID)
    [(equal? (first token-list) 'INT) (match 'INT token-list)]  ; Match an integer
    [(equal? (first token-list) 'LPAR)  ; Match opening parenthesis and process the expression inside
     (match 'RPAR (express (match 'LPAR token-list)))]
    [else (error 'factor "Syntax error in factor")]))  ; Error for invalid factor

; Factor tail grammar
(define (factor-tail token-list)
  (cond
    [(member (first token-list) '(ADD_OP RPAR ID READ WRITE END)) token-list]  ; If token is one of these, we can return the token list as is
    [(equal? (first token-list) 'MULT_OP)  ; Match multiplication operator and continue processing
     (factor-tail (factor (mult-op token-list)))]
    [else (error 'factor-tail
                 (format "Syntax error in factor tail: unexpected token ~a, remaining tokens: ~a"
                         (first token-list) token-list))]))  ; Error if unexpected token in factor tail

; Term grammar
(define (term token-list)
  (cond
    [(member (first token-list) '(ID INT LPAR))  ; If token is an ID, INT, or LPAR, process as a factor
     (factor-tail (factor token-list))]
    [else (error 'term
                 (format "Syntax error in term: expected ID, INT, or LPAR but found ~a, remaining tokens: ~a"
                         (first token-list) token-list))]))  ; Error if invalid term

; Term tail grammar
(define (term-tail token-list)
  (cond
    [(member (first token-list) '(RPAR ID READ WRITE END)) token-list]  ; If token is one of these, we can return the token list
    [(equal? (first token-list) 'ADD_OP)  ; Match addition operator and continue processing
     (term-tail (term (add-op token-list)))]
    [else (error 'term-tail
                 (format "Syntax error in term tail: unexpected token ~a, remaining tokens: ~a"
                         (first token-list) token-list))]))  ; Error if unexpected token in term tail

; Expression grammar
(define (express token-list)
  (cond
    [(member (first token-list) '(ID INT LPAR))  ; If token is ID, INT, or LPAR, process as a term
     (term-tail (term token-list))]
    [else (error 'express
                 (format "Syntax error in expression: unexpected token ~a, remaining: ~a"
                         (first token-list) token-list))]))  ; Error if unexpected token in expression

; Statement grammar
(define (stmt token-list)
  (cond
    [(equal? (first token-list) 'ID)  ; If token is ID, match assignment operation
     (express (match 'ASS_OP (match 'ID token-list)))]
    [(equal? (first token-list) 'READ)  ; If token is READ, match ID
     (match 'ID (match 'READ token-list))]
    [(equal? (first token-list) 'WRITE)  ; If token is WRITE, match expression
     (express (match 'WRITE token-list))]
    [else (error 'stmt
                 (format "Syntax error in statement: unexpected token ~a, remaining: ~a"
                         (first token-list) token-list))]))  ; Error if unexpected token in statement

; Statement list grammar
(define (stmt-list token-list)
  (cond
    [(member (first token-list) '(ID READ WRITE))  ; Process multiple statements
     (stmt-list (stmt token-list))]
    [(equal? (first token-list) 'END) token-list]  ; End of statement list
    [else (error 'stmt-list
                 (format "Syntax error in statement list: unexpected token ~a, remaining: ~a"
                         (first token-list) token-list))]))  ; Error if unexpected token in statement list

; Parse function (starting point)
(define (parse token-list)
  (cond
    [(null? token-list) (error 'parse "Empty token list")]  ; Error if token list is empty
    [(member (first token-list) '(ID READ WRITE))  ; If token is ID, READ, or WRITE, start processing statement list
     (parse (stmt-list token-list))]
    [(equal? (first token-list) 'END) (match 'END token-list)]  ; If token is END, match the end of the input
    [else (error 'parse
                 (format "Unexpected token: ~a, remaining tokens: ~a"
                         (first token-list) token-list))]))  ; Error for unexpected token

; Wrapper to process multiple files
(define (process-files file-names)
  (for-each
   (lambda (file-name)
     (displayln (format "Processing file: ~a" file-name))  ; Print file being processed
     (with-handlers
         ([exn:fail? (lambda (exn)
                       (displayln (format "Error in file ~a: ~a" file-name (exn-message exn)))
                       (displayln "Skipping to the next file..."))])  ; Handle any exceptions (e.g., file read errors)
       (let ([token-list (calc-lexer (open-input-file file-name))])  ; Tokenize the file
         (displayln (format "Token list: ~a" token-list)) ; Optional debugging: print token list
         (parse token-list)  ; Parse the token list
         (displayln "Parsing completed successfully"))))  ; Indicate successful parsing
   file-names))

; Entry point for file processing
(process-files '("input01.txt" "input02.txt" "input03.txt" "input04.txt" "input05.txt"))  ; Process the specified input files