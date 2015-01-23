;;  lexer.ss
;; Ali Tugrul PINAR
;; 111044047
;; A lexer for subset of the scheme language.
;;
;; For more details see the homework description.
;;
;; -------------------------------------------
;; Helper functions ...
;; DELIMETERS with Whitespace characters in characters. (#\space,#\newline, #\page,#\tab , #\linefeed, and #\return) ( PARANTHESIS #\( #\) )
;;INPUT: list of characters.
;;OUTPUT: list of lists of characters. ; 
(define delimeterWhitespaces 
	( lambda(il o1l o2l )
		(cond 
			((eq? il '() ) (cons o1l  o2l ))
			; white space cahracters seperate words.
			((eq? (car il) #\space   ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) )) 
			((eq? (car il) #\newline ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) ))
			((eq? (car il) #\page ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) ))
			((eq? (car il) #\linefeed ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) ))
			((eq? (car il) #\return ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) ))
			((eq? (car il) #\tab     ) (delimeterWhitespaces (cdr il ) '() (cons o1l o2l ) ))
			; separete paranthesises  add space character (cdr il). 
			((eq? (car il) #\(       ) (delimeterWhitespaces (append '( #\space ) (cdr il )) '( #\( ) (cons o1l o2l ) )) 
			((eq? (car il) #\)       ) (delimeterWhitespaces (append '( #\space ) (cdr il ))  '( #\) ) (cons o1l o2l ) )) 
			; control #\"  one character not STRING_LITERAL.
			((eq? (car il) #\#       ) (delimeterWhitespaces (cdrbyCharacter (cdr il) #\space ) '() (cons (getCharacters (cdr il) '() ) o2l )) )
			; delimeter string literal and find last #\" characters and continue delimeters.
			((eq? (car il) #\"       ) (delimeterWhitespaces (cdrbyCharacter (cdr il) #\" ) '() (cons (append  '( #\" )  (delimeterStringLiteral (cdr il) '() )) o2l) ) )
			(else  (delimeterWhitespaces (cdr il) (append  o1l (list (car il))) o2l ))
			)

	))
;; string literal delimeter by "
;;INPUT : list of characters (l), one of elements will be must #\" . helper list (o1l)
;;OUTPUT: list of string literal. (example )(#\" #\d #\i #\s #\p #\" )
(define delimeterStringLiteral 
	(lambda(l o1l)
		(cond 
			((eq? (car l) #\" )  (append o1l '( #\" ) ) )
			( else (delimeterStringLiteral (cdr l) (append o1l (list (car l)))))

		)))

(define getCharacters
	(lambda(l s)
		(cond
			((eq? l '() ) s)
			((char-whitespace? (car l ) ) s )
			(else (getCharacters (cdr l) ( append  s (list (car l)) ) ))
		)))

;  find character " #\" " and return (cdr list )
;INPUT : list of characters (l)
;OUTPUT: list of characters.
(define cdrbyCharacter
	(lambda(l c)
		(cond
			((eq? (car l) c ) (cdr l) )
			((char-whitespace? c )
				(if(char-whitespace? (car l))
					(cdr l) (cdrbyCharacter (cdr l) c) )

			 )
			(else (cdrbyCharacter (cdr l) c))

		)))

; CREATE STRING 
;INPUT: list of lists of characters.
;OUTPUT list of strings.
(define createString 
	(lambda(l)
		(cond 
			((eq? l '() ) '() )
			(else  (append (createString (cdr l) )  (list (list->string (car l))) ))
			)

		))
;; remove  EMPTY STRINGS from LIST OF STRINGS.
;;INPUT: list of strings.
;;OUTPUT: list of strings.
(define stringSplit 
	(lambda(l)
		(cond 
			((eq? l '() ) '() )
			((string=? (list->string (car l)) "") (stringSplit (cdr l)) )
			(else  (append  (stringSplit (cdr l) ) (list (list->string (car l) ) ) ))
			)

		))
; will be named strings by Lexical rules.
;INPUT: list of strings (l), helper lists o1l 
;OUTPUT: list of lists of strings. 
(define namedLexical
	(lambda (l o1l)
		(cond
			((eq? l '() ) o1l )
			((string=? (car l) "(" ) (namedLexical (cdr l) (append o1l (list(cons      "LPAREN          : "  '( "("   ))  ) )))
			((string=? (car l) ")" ) (namedLexical (cdr l) (append o1l (list(cons      "RPAREN          : "  '( ")"   )) ) )))
			((string=? (car l) "lambda" ) (namedLexical (cdr l) (append o1l (list(cons "LAMBDA_KEYWORD  : "  '( "lambda" ) )  ) )))
			((string=? (car l) "define" ) (namedLexical (cdr l) (append o1l (list(cons "DEFINE_KEYWORD  : "  '( "define"   )) ) )))
			((string=? (car l) "or" ) (namedLexical (cdr l) (append o1l (list (cons    "OR_KEYWORD      : "  '( "or" ) ) ) )))
			((string=? (car l) "not" ) (namedLexical (cdr l) (append o1l (list (cons   "NOT_KEYWORD     : "  '( "not" ) )  )) ))
			((string=? (car l) "if" ) (namedLexical (cdr l) (append o1l (list(cons     "IF_KEYWORD      : "  '( "if" ) ) ) )))
			((string=? (car l) "and" ) (namedLexical (cdr l) (append o1l (list(cons    "AND_KEYWORD     : "  '( "and" ) )  ) )))
			((string=? (car l) "quote" ) (namedLexical (cdr l) (append o1l (list(cons  "QUOTE_KEYWORD   : "  '( "quote" ) )  ) )))
			((eq? (typeLex (car l)) 1) (namedLexical (cdr l) (append o1l (list(cons    "BOOLEAN_LITERAL : "  (list (car l )) )   )) ))
 			((eq? (typeLex (car l)) 2) (namedLexical (cdr l) (append o1l (list (cons   "INTEGER_LITERAL : "  (list (car l )) )  ) ) ))
 			((eq? (typeLex (car l)) 3) (namedLexical (cdr l) (append o1l (list (cons   "STRING_LITERAL  : "  (list (car l )) )  ) ) ))
 			((eq? (typeLex (car l)) 4) (namedLexical (cdr l) (append o1l (list (cons   "UNBOUND ! ------> "  (list (car l )) )  ) ) ))
			(else (namedLexical (cdr l) (append o1l (list (cons                        "IDENTIFIER      : "  (list (car l) ) ) ) ) ))
		)))

; DETERMINEN type of lexical string 
;INPUT: string
;OUTPUT: code of type.
(define typeLex
	(lambda(s)
		(cond
			((string=? s "#t") 1 )
			((string=? s "#f") 1 ) ; Boolean literal.
			((integer? (string->number s ) ) 2) ; will be integer literal.
			((eq? (car (string->list s)) #\") (if(eq? (length (string->list s)) 1) 0 3 )) ; will be string literal.
			((number? (char->digit (car (string->list s)))) 4 ) ; not identifier, not number. must be wrong name.  
			(else 0))
		))

;PRINTING LIST with newline.
;INPUT: list of pair of tokenType-lexeme (l1), helper var l2, l3
(define printLexical 
	(lambda(l1 l2 l3)
		(cond
			((eq? l1 '() ) )
			(else (printLexical (cdr l1) (display (car l1)) (display #\newline ) ))
		)))

;PRINTING TOKEN-LEXEME TEST FUNCTION.
;INPUT: list of pair of tokenType-lexeme (l).
(define printTestLexeme 
	(lambda(l)
		(display #\newline )
		(display #\newline )
		( display "  Token Type     -  Lexeme ")
		(display #\newline )
		(printLexical l '() '() )

		))

;; -------------------------------------------
;; LEXER1
;; 
;; Reads a program from the string argument and returns the token type and the corresponding lexeme...

(define lexer1 
  (lambda (p)
  	( namedLexical (stringSplit (delimeterWhitespaces (string->list p) '() '() ) ) '() )
  )
)
;; -------------------------------------------
;; LEXER2
;; 
;; print lexer1 results .
(define lexer2 
	(lambda(p)
		(printTestLexeme (lexer1 p)  )
))

 ;; DEFINE LEXER lexer1 or lexer2


;(define lexer lexer1 )
(define lexer lexer2 )

