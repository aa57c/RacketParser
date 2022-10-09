#lang racket
(require 2htdp/batch-io)
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))


;scanner to be called on input files
;derived from this source: https://matt.might.net/articles/lexers-in-racket/
(define calc-lexer
  (lexer
   ;key words write and read
   ["write"
    (cons `WRITE
          (calc-lexer input-port))]
   ["read"
    (cons `READ
          (calc-lexer input-port))]
   ;reads in each character if present until whitespace is found, creates an ID token
   [(:+ (:or (char-range #\a #\z)(char-range #\A #\Z)))
    (cons `ID
          (calc-lexer input-port))]
   ;token for assignment operator
   [(:: (:? #\:) (:+ #\=))
    (cons `ASS_OP
          (calc-lexer input-port))]
   ;token for both multiplication and division operator
   [(:or #\* #\/)
    (cons `MULT_OP
          (calc-lexer input-port))]
   ;end token
   [(:+ (:or #\$ #\$))
    (cons `END
          (calc-lexer input-port))]
   ;left parenthesis token
   [#\(
    (cons `LPAR
          (calc-lexer input-port))]
   ;right parenthesis token
   [#\)
    (cons `RPAR
          (calc-lexer input-port))]
   ;integer/number token
   [(:: (:? #\-)(:+ (char-range #\0 #\9)))
        (cons `INT
              (calc-lexer input-port))]
   ;subtraction/addition token
   [(:or #\+ #\-)
    (cons `ADD_OP
          (calc-lexer input-port))]
   ;white spaces are ignored
   [whitespace
    (calc-lexer input-port)]
   [(eof)
    '()]))



#|
all functions beyond this point were derived from the class textbook (pg 74-76)
|#

;match function to check if the token is valid, if end token is found, program ends for specific input file
;otherwise throw a message saying there was a syntax error
(define (match expected token_list)
  (cond
    [(equal? expected 'END) (displayln "valid tokens")]
    [(equal? expected (first token_list)) (rest token_list)]
    [else displayln "failed. syntax error found"]))



;parse function to start program
(define (parse token_list)
  ;checks to see if the first token is an ID, read, or write key word
  (cond
    [(or (equal? (first token_list) 'ID)
         (equal? (first token_list) 'READ)
         (equal? (first token_list) 'WRITE))
     ;if so then call statement list grammar, then recursively call parse again 
     (parse(stmt_list token_list))]
    ;handles the end token towards the end of the program
    [(equal? (first token_list) 'END) (match 'END token_list)]
    [else displayln "failed. syntax error found"]))
;statement list grammar
(define (stmt_list token_list)
  (cond
    [(or (equal? (first token_list) 'ID)
         (equal? (first token_list) 'READ)
         (equal? (first token_list) 'WRITE))
     (stmt_list(stmt token_list))]
    [(equal? (first token_list) 'END) token_list]
    [else displayln "failed. syntax error found"]))
;statment grammar
(define (stmt token_list)
  (cond
    #|if first token is ID, first match the ID token and remove from the list,
    then assume assignment operator is next then match that. finally, call expression function|#
    [(equal? (first token_list) 'ID) (express(match 'ASS_OP(match 'ID token_list)))]
    #|if first token is equal to read, match the read and id token after it, remove both from the list|#
    [(equal? (first token_list) 'READ) (match 'ID(match 'READ token_list))]
    #|if first token is equal to write, match the write token and call expression function|#
    [(equal? (first token_list) 'WRITE) (express(match 'WRITE token_list))]
    #|if syntax error was found at this point, throw that message into the console|#
    [else displayln "failed. syntax error found"]))
;expression grammar
(define (express token_list)
  ;checks to to see if first token is an id, an int, or a left parenthesis
  (cond
    [(or (equal? (first token_list) 'ID)
         (equal? (first token_list) 'INT)
         (equal? (first token_list) 'LPAR))
     ;if so call term expression first, then call term_tail
     (term_tail(term token_list))]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))

;term tail grammar
(define (term_tail token_list)
  (cond
    ;if it's any of these...
    [(or (equal? (first token_list) 'RPAR)
         (equal? (first token_list) 'ID)
         (equal? (first token_list) 'READ)
         (equal? (first token_list) 'WRITE)
         (equal? (first token_list) 'END))
     ;skip (return the same list, these will later be matched and removed in other functions in this program)
     token_list]
    ;if the first token is an add or subtraction sign, call the addition operator function then term, then term_tail
    [(equal? (first token_list) 'ADD_OP) (term_tail(term(add_op token_list)))]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))
;term grammar
(define (term token_list)
  ;if the first token is any of these...
  (cond
    [(or (equal? (first token_list) 'ID)
         (equal? (first token_list) 'INT)
         (equal? (first token_list) 'LPAR))
     ;then call factor, then factor tail
     (factor_tail(factor token_list))]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))
;factor tail grammar
(define (factor_tail token_list)
  ;if the first token are any of these...
  (cond
    [(or (equal? (first token_list) 'ADD_OP)
         (equal? (first token_list) 'RPAR)
         (equal? (first token_list) 'ID)
         (equal? (first token_list) 'READ)
         (equal? (first token_list) 'WRITE)
         (equal? (first token_list) 'END))
     ;skip (return the same list, these will later be matched and removed in other functions in this program)
     token_list]
    ;if syntax error was found at this point, throw that message into console
    [(equal? (first token_list) 'MULT_OP) (factor_tail(factor(mult_op token_list)))]
    [else displayln "failed. syntax error found"]))
;factor grammar
(define (factor token_list)
  (cond
    [(equal? (first token_list) 'ID) (match 'ID token_list)]
    [(equal? (first token_list) 'INT) (match 'INT token_list)]
    [(equal? (first token_list) 'LPAR) (match 'RPAR(express(match 'LPAR token_list)))]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))

(define (add_op token_list)
  (cond
    [(equal? (first token_list) 'ADD_OP) (match 'ADD_OP token_list)]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))

(define (mult_op token_list)
  (cond
    [(equal? (first token_list) 'MULT_OP) (match 'MULT_OP token_list)]
    ;if syntax error was found at this point, throw that message into console
    [else displayln "failed. syntax error found"]))


#|
Note for Mr. Hare,
Sorry for the inconvenice. I ran out of time trying to decide how to read in all the files at once without calling them like this below.
I have tried looping and recursive calling. But I had some sort of scope issue where open-input-file was not working within the functions that
I have wrote above. It didn't like the parameter I passed into it (the file string) for some reason. Even though it was a string. But this was the best
I could come up with. Debug really shows how each input goes through the scanner and parser. However, my parser works as well as my scanner, which hopefully
is something you can work with.

I found this project really challenging. I have never done anything like this before. But I am glad I did because now I have a pretty good idea about how a part
of the compiler works. 
|#



(define token_list(calc-lexer(open-input-file "input01.txt")))
(parse token_list)
(define token_list2(calc-lexer(open-input-file "input02.txt")))
(parse token_list2)
(define token_list3(calc-lexer(open-input-file "input03.txt")))
(parse token_list3)
(define token_list4(calc-lexer(open-input-file "input04.txt")))
(parse token_list4)
(define token_list5(calc-lexer(open-input-file "input05.txt")))
(parse token_list5)












  

  

  
    
    
    
    
    
   
    

    
    
  

   
  

    

    


    

    
    
    
     
     
   






       





               
   
  








