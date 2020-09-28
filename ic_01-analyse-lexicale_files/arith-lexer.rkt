#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))



(define-empty-tokens Toks
  ;;ajouter les tokens
  (AND OR NOT OPAR CPAR ASSIGN SEMICOL Eof ADDITION SOUSTRACTION MULTIPLICATION DIVISION MODULO PUISSANCE))

;; les identifiants de tokens

(define-tokens Identifiers
  (Id))





(define tokenize
  (lexer-src-pos
   ((eof) (token-Eof))
   (whitespace (tokenize input-port))
   ("&&" (token-AND)) ;et
   ("||" (token-OR)) ; ou
   ("!" (token-NOT)) ; non
   ("(" (token-OPAR)) ; parenthese ouvrante
   (")" (token-CPAR)) ; parenthèse fermée
   ("=" (token-ASSIGN)) ; assigne
   (";" (token-SEMICOL)) ; point virgule de fin
   ("+" (token-ADDITION)) ; signe de l'addition
   ("-" (token-SOUSTRACTION)) ; signe de la soustraction
   ("*" (token-MULTIPLICATION)) ; signe de la multiplication
   ("/" (token-DIVISION)) ; signe de la division
   ("%" (token-MODULO)) ; signe du modulo
   ("**" (token-PUISSANCE)); signe de puissance 
   ((:+ alphabetic) (token-Id lexeme)) ; reconnaître l'alphabet
   ((:+ numeric) (token-Id lexeme)) ; reconnaître les numéros/chiffres 
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