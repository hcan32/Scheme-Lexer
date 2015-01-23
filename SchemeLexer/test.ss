;;
;; Code for testing the Scheme lexer.
;;

(load "pinar_ali_tugrul_111044047_lexer.ss")

(define test1 (lambda () (lexer test_prog_3)  ))


;; When the lexer is called with the following program as argument, the result should be a list of tokens with:
;; ( (LPAREN "(") (IDENTIFIER "+") (IDENTIFIER "1") (IDENTIFIER "y") (RPAREN ")") )
(define test_prog_1 "(+ 1 y)")


(define test_prog_2 "(define factorial1 (lambda (n) 12abd (if (= n 1) \" Heyy Scheme \"1 (+ n (factorial1 (- n 1))))))(display \" This is Scheme \"))")

(define test_prog_3 "(define delimeterStringLiteral 
	(lambda(l o1l)
		(cond 
			((eq? (car l) #\# )  (append o1l '( #\" ) ) )
			( else (delimeterStringLiteral (cdr l) (append \" Scheme \" (list (car l)))))

		)))")


