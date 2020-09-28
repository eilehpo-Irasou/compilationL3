#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))



(define-empty-tokens Toks
  ;;ajouter les tokens
  (AND OR NOT OPAR CPAR ASSIGN SEMICOL Eof))

;; les identifiants de tokens

(define-tokens Identifiers
  (Id))





(define tokenize
  (lexer-src-pos
   ((eof) (token-Eof))
   (whitespace (tokenize input-port))
   ("&&" (token-AND))
   ("||" (token-OR))
   ("!" (token-NOT))
   ("(" (token-OPAR))
   (")" (token-CPAR))
   ("=" (token-ASSIGN))
   (";" (token-SEMICOL))
   ((:+ alphabetic) (token-Id lexeme))
   ))


(define (lex input)
  (port-count-lines! input)
  (let ((token (tokenize input)))
    (unless (eq? (position-token-token token ) 'Eof)
      (write token)
      (newline)
      (lex input))))



(define argv (current-command-line-arguments))
(cond
  ((= (vector-length argv) 1)
   (call-with-input-file (vector-ref argv 0) lex))
  (else
   (eprintf "Usage: racket arith-lexer.rkt <input file>\n")
   (exit 1)))