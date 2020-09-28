#lang racket
;; exercice 2 question 1

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
(define first-lexer
  (lexer
   ((eof) 'fini) 
   (whitespace (first-lexer input-port))
   ((:+ alphabetic) (begin (write lexeme)
                           (first-lexer input-port)))

   ((:: any-char ) (begin ( write lexeme)
                          (first-lexer input-port)))
   ))

;; exercice 2 question 2
(call-with-input-string "Est-ce que ça marche ?" first-lexer)


;exercice 2 question 3
(define second-lexer
  (lexer
   ((eof) 'fini)
   (whitespace (second-lexer input-port))
   ((:+ alphabetic) lexeme)
   ((:: any-char) lexeme)))

;; ne va afficher que "Est"
;;(call-with-input-string "Est ce que ça marche ?" second-lexer)

;; exercice 2 question 4

(define (second-lex input);; input pour l'entrée
  (let((token(second-lexer input)))
    (unless (eq? token 'fini)
      (write token)
      (second-lex input)
      (newline))))


;; exercice 2 question 5

(call-with-input-string "Est-ce que ça remarche ?" second-lex)

(define argv (current-command-line-arguments))
(cond
  ((= (vector-length argv) 1)
   (call-with-input-file (vector-ref argv 0) second-lex))
  (else
   (eprintf "Usage: racket lexer.rkt \"file.txt\"\n")
   (exit 1)))
