(define-library (niyarin regexp)
  (import (scheme base))
  (export string->regexp)
  (begin

(cond-expand 
  (gauche (import (scheme base) (scheme char) (scheme cxr) (scheme write) (scheme read) (srfi 1)))
  (picrin (import (scheme base) (scheme cxr)(scheme write)(scheme read)(srfi 1)))
  (else 
    (import (scheme base) (scheme char) (scheme cxr) (scheme write) (scheme read) )

    ;;SRFI1
    (define (filter pred ls)
      (let loop ((l ls))
        (cond 
          ((null? l) '())
          ((pred (car l)) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))))

  (define (iota count . start&step)
     (let-values 
       (((start step) 
        (cond 
          ((null? start&step) (values 0 1))
          ((null? (cdr start&step)) (values (car start&step) 1))
          (else  (values (car start&step) (cadr start&step))))))
       (let loop ((i count)(cnt start))
         (if (zero? i) 
           '()
           (cons cnt (loop (- i 1) (+ cnt step)))))))
    ))




(define *on-the-fly-number* 10)

(define-syntax TOKEN-CASE
  (syntax-rules (else IN-BRACKET) 
    ((_ (i1 i2 flag) ) #f)
    ((_ (i1 i2 flag)  (IN-BRACKET body) after ... )
     (if flag 
       body
       (TOKEN-CASE (i1 i2 flag) after ... )))
    ((_ (i1 i2 flag)(else body)) body) 
    ((_ (i1 i2 flag) (c1 c2 body) after ...)
     (if (and (char=? i1 c1)(char=? i2 c2))
       body
       (TOKEN-CASE (i1 i2 flag) after ...)))
    ((_ (i1 i2 flag) (c1 body) after ...)
     (if (char=? i1 c1) 
       body
       (TOKEN-CASE (i1 i2 flag) after ... )))))
     

(define (regex-token-nize reg)
  (let ((len (string-length reg)))
    (let loop ((p 0)(ret '())(bracket-flag #f))
      (if (< p (- len 1))
        (let ((c1 (string-ref reg p))
              (c2 (string-ref reg (+ p 1))))
           (TOKEN-CASE
             (c1 c2 bracket-flag)
             (#\: #\] (loop (+ p 2) (cons 'rbracket: ret) #f))
             (#\[ #\: (loop (+ p 2) (cons 'lbracket: ret) #t))
             (#\] (loop (+ p 1)(cons 'rbracket ret) #f))
             (IN-BRACKET (loop (+ p 1) (cons c1 ret) bracket-flag))
             (#\\ (loop (+ p 2)(cons c2 ret) bracket-flag))
             (#\* #\? (loop (+ p 2)(cons '_*? ret) bracket-flag))
             (#\+ #\? (loop (+ p 2)(cons '_+? ret) bracket-flag))
             (#\? #\? (loop (+ p 2)(cons '_?? ret) bracket-flag))
             (#\+ (loop (+ p 1)(cons 'plus ret) bracket-flag));supported
             (#\* (loop (+ p 1)(cons 'star ret) bracket-flag));supported
             (#\? (loop (+ p 1)(cons 'question ret) bracket-flag));supported
             (#\[ (loop (+ p 1)(cons 'lbracket ret) #t));supported
             (#\{ (loop (+ p 1)(cons 'lbrace ret) bracket-flag));supported
             (#\} (loop (+ p 1)(cons 'rbrace ret) bracket-flag));supported
             (#\^ (loop (+ p 1)(cons '^ ret) bracket-flag))
             (#\$ (loop (+ p 1)(cons '$ ret) bracket-flag))
             (#\| (loop (+ p 1)(cons 'vertical-bar ret) bracket-flag));supported 
             (#\. (loop (+ p 1)(cons 'dot ret) bracket-flag));supported
             (#\( (loop (+ p 1)(cons 'lparen ret) bracket-flag));suuported
             (#\) (loop (+ p 1)(cons 'rparen ret) bracket-flag));supported
            
             (else (loop (+ p 1)(cons c1 ret) bracket-flag))))
        ret
        ))))

(define (regex-expander-phase1 tokens) 
  ;bracket brace 等をまとめる
  ;返り値は展開結果と次の入力の多値で返す

  (define (catch-number tkns num)
    (if (or (null? tkns) (not (and (char? (car tkns)) (char-numeric? (car tkns)))));読み込みが終端か非数字
      (values (string->number (list->string num)) tkns)
      (catch-number (cdr tkns) (cons (car tkns) num ))))
  
  (define (brace-expander tkns)
    (let-values (((ret next)
      (let loop ((tk tkns)(ret '()))
        (cond 
          ((and (char? (car tk)) (char-numeric? (car tk)))
             (let-values (((number next) (catch-number tk '())))
                 (loop next (cons number ret))))
          ((eqv? (car tk) #\,) 
             (loop (cdr tk) (cons (car tk) ret)))
          ((eqv? (car tk) 'lbrace) (values ret (cdr tk))  )))))
      
      (case
        (length ret)
        ((3) 
            (if (and (number? (car ret))(number? (caddr ret)) (eqv? #\, (cadr ret)))
              (values (list 'brace (car ret)(caddr ret)) next)))
        ((2) 
           (cond 
             ((and (eqv? (car ret) #\,) (number? (cadr ret)))
                (values (list 'brace 0 (cadr ret)) next))
             ((and (eqv? (cadr ret) #\,) (number? (car ret)))
                (values (list 'brace (car ret) -1) next))
             (else (error "invalid regep error"))))
        ((1)
          (values (list 'brace (car ret) (car ret)) next))  

        (else (error "invalid regexp error")))
      ))


  (define (bracket-expander: tkns) 
    (let loop ((tk tkns)(name '()))
      (if (eqv? (car tk) 'lbracket:)
        (let ((name (list->string name)))
          (values (list 'bracket: name) (cdr tk)))
        (loop (cdr tk) (cons (car tk) name)))))
  
 


  (define (bracket-expander tkns)
    ;(bracketが閉じられないケースは、前の段階で潰せているのでチェック不要)
    (let loop ((tk tkns)(ret '()))
      (cond 
         ((eqv? (car tk) 'rbracket:)
          (let-values (((o next) (bracket-expander: (cdr tk))))
              (loop next (cons o ret))))
         ((eqv? (car tk) 'lbracket)
           (values (cons 'bracket ret) (cdr tk)))
         ((and (not (null? (cdr tk)) )
               (eqv? (cadr tk) #\-)
               (not (null? (cddr tk))))
            (loop (cdddr tk) (cons (cons (car tk) (caddr tk)) ret )));a-z、A-Z、0-9とかのハイフン
         (else 
           (loop (cdr tk) (cons (car tk) ret) )))))


  (let loop ((tkns tokens)(ret '()))
    (if (null? tkns) 
      ret
      (case (car tkns)
        ((rbrace) 
           (let-values (((o next)(brace-expander (cdr tkns))))
                       (loop next (cons o ret))))
        ((rbracket:) 
           (let-values (((o next)(bracket-expander: (cdr tkns))))
                       (loop next (cons o ret))))
        ((rbracket)
           (let-values (((o next) (bracket-expander (cdr tkns))))
                       (loop next (cons o ret))))
        ((rparen)
          (let-values  (((o next) (loop (cdr tkns) '())))
                (loop next (cons (cons 'paren o ) ret))))
        ((lparen)
         (values ret (cdr tkns)))
        (else (loop (cdr tkns) (cons (car tkns) ret))
        )))))




 (define (hyphen-bracket-expander start end)
    (define (check-char-type chr)
      ;文字のチェック
      ;大文字1
      ;小文字2
      ;数字3
      ;それ以外0
      (cond 
        ((char-numeric? chr) 3)
        ((and (char-alphabetic? chr) (char-upper-case? chr)) 1)
        ((and (char-alphabetic? chr) (char-lower-case? chr)) 2)
        (else 3)))
      (let ((stype (check-char-type start))
            (etype (check-char-type end))
            (end-num (char->integer end)))
          (unless (= stype etype) (error (string-append "ERROR::invalid regexp " (string start #\- end))))
            (let loop ((i (char->integer start)))
              (if (< i end-num)
                (list 'vertical-bar (list (integer->char i)) (list (loop (+ i 1))))
                (integer->char i) ))))



(define (regex-expander-phase2 tokens)
  (let loop ((tk tokens)(ret '())) 
    (cond 
      ((null? tk) ret)
      ((pair? (car tk)) 
       (case (caar tk)
         ((paren) (loop (cdr tk) (cons (cons 'paren (loop (cdar tk)'() )) ret)))
         ((bracket) 
          (loop (cdr tk)
                (cons 
                  (if (eqv? (cadar tk) #\^)
                     (error "ERROR:sorry unsupported regexp ");否定ケース [^ ]
                     (let loop2 ((l (cdar tk)))
                       (let ((evaled-l (if (pair? (car l)) (hyphen-bracket-expander (cdar l) (caar l)) (car l))))
                         (if (null? (cdr l))
                           evaled-l;car l がpairの場合展開が必要
                           ( list 'vertical-bar (list evaled-l) (list (loop2 (cdr l)))))
                       )
                       )
                    )
                  ret)))
         ((brace)
          (let ((min-number (cadar tk))
                (max-number (caddar tk)))
            (cond 
              ((= max-number -1) (error "too-large-states-error"));定数個+starに変換。 あとでやる
              ((> max-number *on-the-fly-number*)(error "too-large-states-error"));on-the-fly あとでやる
              (else 
                ;max-number != min-number
                (let 
                    ;min-number-1個か0個の連結に変換する。
                  ((a (cons 'paren (map (lambda (x) (car ret)) (iota (max (- min-number 1) 0) ))))
                   ;max-number - min-number個をvertical-varに変換する。
                   (b (let loop ((i (- min-number 1)))
                        (if (>= i (- max-number 1))
                          (cons 'paren (map (lambda (j) (car ret)) (iota i) ))
                          (list 'vertical-bar (list (cons 'paren (map (lambda (j) (car ret)) (iota i)))) (list (loop (+ i 1))))))))
                   (loop (cdr tk) (cons a (cons b (cdr ret)))))
                )))
            )
         (else (loop (cdr tk) (cons (car tk) ret)))
         ))
      ((symbol? (car tk)) 
       (case (car tk)
         ((vertical-bar) 
          (let ((tail (loop (cdr tk) '())))
            (list (list 'vertical-bar ret tail))))
         ((star)
            (loop (cdr tk) (cons (list 'star (car ret)) (cdr ret))))
         ((plus)
          (loop (cdr tk) (cons (list 'star (car ret)) ret)))
         ((question)
          (loop (cdr tk) (cons (list 'vertical-bar (list (car ret)) '()) (cdr ret))))
         (else (loop (cdr tk) (cons (car tk) ret)))
         ))
      (else (loop (cdr tk) (cons (car tk) ret )))
      )))
    


(define (reg->e-nfa reg)
  (let loop ((reg reg)(nfa 'match ))
    (cond 
      ((null? reg) nfa)
      ((pair? (car reg))
       (case (caar reg)
         ((vertical-bar) 
          (loop (cdr reg) (list '() (loop (cadar reg) nfa)(loop (caddar reg) nfa   ) )))
         ((paren) 
            (loop (cdr reg)
                  (loop (cdar reg) nfa)))
         ((star)
          (let* ((head-cell (list '() ))
                (tail-cell (list '() nfa)))
            (let ((next (loop (cdar reg) head-cell)))

              (set-cdr! head-cell (list next tail-cell ))
              (loop (cdr reg) head-cell )  )))
         (else (error "UNDEFINED SYMBOL " (caar reg)))
         ))
      (else 
          (loop (cdr reg)(list (car reg ) nfa  ))
        ))))


(define (e-nfa->nfa e-nfa ) 
  (define not-epsilon-transition? (lambda (x) (and (pair? x) (not (null? (car x))))))
  (define (cut-epsilon e-nfa) 
    ;ε 移動でいける状態を統合したものを返す。
    (let loop ((e-nfa e-nfa))
      (cond 
        ((null? e-nfa) '())
        ((null? (car e-nfa)) 
         (let ((a (filter not-epsilon-transition? (cdr e-nfa))))
           (if (null? a)
             (apply append (map (lambda (x) (if (null? x) '(()) (loop x)) )
                                (cdr e-nfa)))
             (append  a
                   (apply append (map (lambda (x) (if (null? x) '(()) (loop x)) )
                                      (cdr e-nfa)))))))
        (else  '()))))

  (let ((cnt 0))
    (values 
      (let loop ((e-nfa e-nfa)(used '()))
        (cond 
          ((null? e-nfa) '())
          ((assv e-nfa used) => cdr)
          (else 
            (set! cnt (+ cnt 1))
            (let ((cell (cons (car e-nfa) '())))
              (set-cdr!
                cell 
                  (let loop2 ((ts (cdr e-nfa)))
                    ;状態遷移にε 移動があるか調べて場合分けする。
                    (cond 
                      ((null? ts) '())
                      ((eqv? (car ts) 'match) (cons 'match (loop2 (cdr ts))))
                      ((not-epsilon-transition? (car ts)) 
                       (cons (loop (car ts) (cons (cons e-nfa cell) used))
                             (loop2 (cdr ts))))
                      (else  
                        (let ((appended-transisions 
                                (map (lambda (x) (loop x (cons (cons e-nfa cell) used)))
                                     (cut-epsilon (car ts)))))
                           (if (null? appended-transisions)
                             (cons appended-transisions (loop2 (cdr ts)))
                             (append appended-transisions (loop2 (cdr ts)))
                             )
                        )))
                    )
                  )
               cell
            ))))
      cnt)
    ))



(define (integer-list-insert! integer-list integer)
  ;おなじものを削除して順番を維持して破壊的に挿入
  (let loop ((last-cell '())(l integer-list))
    (cond
      ((and (null? l) (null? last-cell)) (list integer)) 
      ((null? l) (set-cdr! last-cell (list integer)) integer-list )
      ((> integer (car l))(loop l (cdr l)))
      ((= integer (car l)) integer-list)
      ((null? last-cell) (cons integer integer-list))
      (else (set-cdr! last-cell (cons integer l)) integer-list ))))

  


(define (nfa->dfa nfa size)
  ;nfaからdfaに変換する。

  ;"."(任意の文字)をうまいことやる
  (define (get-dot tr)
    (let loop ((tr tr)(states '())(dot-symbols '())(dot-states '())(ep-cnt #t))
      (cond
        ((null? tr) (values states dot-symbols dot-states))
        ((eqv? (caar tr) 'dot)(loop (cdr tr) states (append (cadar tr) dot-symbols) (integer-list-insert! dot-states (cddar tr)) ep-cnt))
        ((and (null? (caar tr)) ep-cnt)
           (loop (cdr tr)(cons (car tr) states) dot-symbols dot-states #f))
        ((null? (caar tr)) 
         (loop (cdr tr) states dot-symbols dot-states #f))
        (else (loop (cdr tr) (cons (car tr) states) dot-symbols dot-states ep-cnt)))))

  (define (search-non-deterministic tgt states n-init)
    (let loop ((states states) (n-trans n-init)(d-trans '()))
      (cond
        ((null? states) (values n-trans d-trans))
        ((eqv? (caar states) tgt) (loop (cdr states) (cons (cdar states) n-trans) d-trans))
        (else (loop (cdr states) n-trans (cons (car states) d-trans))))))



  (let ((dfa (make-vector (* size 2)))
        (s-id 1)
        (used (make-vector (* size 2) #f ))
        (combs '()))
    (define (update-pos)
      (set! s-id (+ s-id 1)) 
      (when (= s-id (vector-length dfa))
        (begin
          (set! dfa (vector-append dfa (make-vector (* size 2))))
          (set! used (vector-append used (make-vector (* size 2 ) #f)))))
      s-id
    )
    ;nfaをvector-baseに変える。
    (vector-set! dfa 1 (list (cons (car nfa) 2)))
    (vector-set! dfa 0 (list (cons '() 0)))

    (let ((memo '()))
      (let loop0 ((nfa nfa))
        (cond 
          ((null? nfa) '())
          ((assv nfa memo) '())
          ((eqv? nfa 'match) '())
          ((and (null? (cddr nfa)) (eqv? (cadr nfa ) 'match)) (set! memo (cons (cons nfa 0) memo)))
          (else 
            (let ((state (update-pos)))
              (set! memo (cons (cons nfa state) memo))
              (for-each loop0 (cdr nfa))
              (vector-set! dfa
                           state
                           (map 
                              (lambda (x) 
                                (if (eqv? (car x) 'dot)
                                  (cons (car x) (cons '() (cdr (assv x memo))))
                                  (cons (car x )(cdr (assv x memo))))) 
                              (cdr nfa)))
              )))))
    ;dot-state
    (let loop0 ((pos 1))
      (cond
        ((= pos 0) '())
        ((vector-ref used pos)'())
        (else
          (let-values (((tr dot-symbols dot-state)(get-dot (vector-ref dfa pos))))
              (let loop ((tr tr)(state '()))
                (cond
                  ((null? tr)
                      (let* ((dot-update #f)
                             (new-state 
                                (if (null? dot-state)
                                  state
                                  (let ((ct (assoc dot-state combs)))
                                    (set! dot-symbols (append dot-symbols (map car state)))
                                    (cond 
                                      ((null? (cdr dot-state))
                                         (set! dot-update (car dot-state))
                                         (cons (cons 'dot (cons dot-symbols (car dot-state))) state)) ;"."の遷移の統合なしケース
                                      (ct (cons (cons 'dot (cons dot-symbols (cdr ct))) state));統合したが、登録済みだった(再更新不要)ケース
                                      (else 
                                        (let ((p (update-pos))
                                              (d-tr 
                                                    (let f ((s dot-state))
                                                      (if (null? s )
                                                        '()
                                                        (append (vector-ref dfa (car s)) (f (cdr s)))))))

                                          (set! dot-update p)
                                          (vector-set! dfa p d-tr)
                                          (cons (cons 'dot (cons dot-symbols p)) state))))))))
                        (vector-set! dfa pos new-state)
                        (vector-set! used pos #t)
                        (for-each 
                          (lambda (x) (loop0 (cdr x)))
                          state)
                        (when dot-update (loop0 dot-update))
                      )
                   )
                  (else 
                    (let ((dot-expand (if (memv (caar tr) dot-symbols) '() dot-state)))
                      (let-values (((n-trans d-trans)(search-non-deterministic (caar tr) (cdr tr) dot-expand)))
                           (if (null? n-trans)
                             (loop (cdr tr) (cons (car tr) state ))
                             (let ((comb-tr '()))
                                 (for-each
                                     (lambda (x) (set! comb-tr (integer-list-insert! comb-tr x)))
                                     (cons (cdar tr) n-trans))
                                 (let ((created-id  (cond  ((assoc comb-tr combs) => cdr )(else #f))))
                                     (if created-id
                                       (loop d-trans (cons (cons (caar tr) created-id) state));すでに作られた状態だったケース
                                       (let ((p (update-pos)))
                                         (set! combs (cons (cons comb-tr p) combs))
                                         (vector-set! 
                                           dfa 
                                           p
                                           (let f ((trs comb-tr))
                                             (if (null? trs)
                                               '()
                                               (append (vector-ref dfa (car trs) ) (f (cdr trs))))) )
                                         (loop d-trans (cons (cons (caar tr) p) state))))
                                     ))
                             ))))
                  )
)))))

    (let loop ((i s-id))
      (if (zero? i)
        '()
        (let ((trans (vector-ref dfa i)))
          (when  (and (vector-ref used i) (eqv? (caar trans) 'dot)) 
            (vector-set! 
              dfa
              i
              (reverse (cons (cons 'dot (cddar trans)) (cdr trans)))))

          (unless (vector-ref used i) (vector-set! dfa i #f))
          (loop (- i 1)))))
    dfa
    )
  )





(define (gen-match-procedure dfa)
  (lambda (input . opt)
    ;先頭からマッチさせる
    (define (match-before input)
      (let loop ((input input)
                 (state 1))
        (cond 
          ((= state 0) input)
          ((null? input) 
           (let f ((l (vector-ref dfa state)))
             (cond 
               ((null? l) #f)
               ((null? (caar l)) input)
               (else (f (cdr l))))))

          (else 
            (let loop2 ((tr (vector-ref dfa state)))
              (cond 
                ((null? tr) #f)
                ((null? (caar tr)) (loop input (cdar tr)))
                ((eqv? (caar tr) 'dot) (loop (cdr input) (cdar tr)))
                ((eqv? (caar tr) (car input)) (loop (cdr input) (cdar tr)))
                (else (loop2 (cdr tr)))))) )))


    (define (split input)
      (let loop ((input input)(prev '())(split-res '()))
        (if (null? input) 
         (reverse (cons (list->string (reverse prev)) split-res))
        (let ((ret (match-before input)))
          (cond 
            ((not ret) (loop (cdr input) (cons (car input) prev) split-res))
            ((null? ret) (cons prev split-res))
            (else (loop ret '() (cons (list->string (reverse prev)) split-res))))))))
            
    
    (cond 
      ((null? opt) (let ((ret (match-before (string->list input)))) (if ret (null? ret) ret)))
      ((eqv? (car opt) 'split) (split (string->list input)))
      (else '())
      )))



(define (string->dfa reg)
  (let ((tokens 
          (regex-token-nize (string-append reg " "))))
    (let ((tokens-phase1 (regex-expander-phase1 tokens)))
      (let ((tokens-phase2 (regex-expander-phase2  tokens-phase1 )))
        (let ((e-nfa (reg->e-nfa tokens-phase2)))
          (let-values (((nfa size) (e-nfa->nfa e-nfa)))
            (let ((dfa (nfa->dfa nfa size)))
              dfa)
    ))))))




;exported procedures
(define (string->regexp str)
  (gen-match-procedure (string->dfa str)))


))
