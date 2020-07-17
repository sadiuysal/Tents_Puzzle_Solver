#lang scheme
; 2015400162



(require racket/set)
; Solver function
(define (TENTS-SOLUTION info) (if (empty? (caddr info)) #f
                                  (RETURN-FIRST-NOT-FALSE_solvable solvable  (candidateUpdate  (findCandidates (caddr info)) (car info) (cadr info) (caddr info) '() '() ) (list (candidateUpdate  (findCandidates (caddr info)) (car info) (cadr info) (caddr info) '() '() ) (car info) (cadr info) (caddr info) '() (apply + (car info))  (apply + (car info)) ) ) ))

(define (REPLACE-NTH ourList index value ) (if (not (pair? (cdr ourList))) (if (= 1 index) (list value)
                                                                               (list (car ourList)))
                                               (if (= 1 index) (append (list value) (REPLACE-NTH (cdr ourList) (- index 1) value ))
                                                   (append (list (car ourList)) (REPLACE-NTH (cdr ourList) (- index 1) value ))) ) )

; Helper functions
(define (RETURN-FIRST-NOT-FALSE function ourList) (if (not (pair? ourList)) #f (if (false? (apply function (list (car ourList)))) (RETURN-FIRST-NOT-FALSE function (cdr ourList)) (apply function (list (car ourList))))))


(define (ADJACENT point1 point2) (adj (car point1) (car (cdr point1)) (car point2) (car (cdr point2))))

(define (ADJACENT-WITH-LIST point  pointList) (if (empty? pointList) #f  (if (ADJACENT point (car pointList)) #t (ADJACENT-WITH-LIST point (cdr pointList)))))


(define (NEIGHBOR-LIST grid ) ( nList (car grid) (car (cdr grid)) ))

;My helpers
;helper for adjacent
(define (adj i1 j1 i2 j2) (if (= i1 i2) (if (> 2 (abs (- j1 j2))) #t #f)
                              (if (= j1 j2) (if (> 2 (abs (- i1 i2))) #t #f)
                                  (if (< (+ (abs (- i1 i2)) (abs (- j1 j2)) ) 3) #t #f ))))

;helper for neighbour list
(define (nList i j) (list (list (+ i 1) j) (list (- i 1) j) (list i (- j 1)) (list i (+ j 1))) )


;Returns first not false got given solvable func.
(define (RETURN-FIRST-NOT-FALSE_solvable function lst lstParam) (if (not (pair? lst)) #f
                                                                    (if (false? (function  (car lst) (first lstParam) (second lstParam) (third lstParam) (fourth lstParam) (fifth lstParam) (sixth lstParam) (seventh lstParam) ) )
                                                                        (RETURN-FIRST-NOT-FALSE_solvable function (cdr lst) lstParam)
                                                                        (function  (car lst) (first lstParam) (second lstParam) (third lstParam) (fourth lstParam) (fifth lstParam) (sixth lstParam) (seventh lstParam) )  )))


;recursive solver function 
(define (solvable candidate candidates rowCounts colCounts treeLocs tentLocs remTentCount totalTentCount)(if (< (length candidates) remTentCount) #f
                                                                                                 (if (= 0 remTentCount)   tentLocs  ;
                                                                                                    (let ([lst (fillTents
                                                                                                                (candidateUpdate candidates
                                                                                                                                  (updateRowCounts candidate rowCounts)
                                                                                                                                  (updatecolCounts candidate rowCounts)
                                                                                                                                  treeLocs
                                                                                                                                  (append tentLocs (list candidate))
                                                                                                                                  '())
                                                                                                                (updateRowCounts candidate rowCounts)
                                                                                                                (updatecolCounts candidate colCounts)
                                                                                                                (car (fillCandidateInfos candidate candidates (build-list (length rowCounts) (lambda (x) (* x 0))) (build-list (length colCounts) (lambda (x) (* x 0)))))
                                                                                                                (cadr (fillCandidateInfos candidate candidates (build-list (length rowCounts) (lambda (x) (* x 0))) (build-list (length colCounts) (lambda (x) (* x 0)))))
                                                                                                                (append tentLocs (list candidate))) ])
                                                                                                     (if (and (= (length (caddr lst)) totalTentCount ) (Isfinished treeLocs (caddr lst))) (caddr lst)
                                                                                                         (if (empty? (candidateUpdate candidates
                                                                                                                                (car lst)
                                                                                                                                (cadr lst)
                                                                                                                                treeLocs
                                                                                                                                (caddr lst)
                                                                                                                                '())) #f
                                                                                                                                      
                                                                                                            (solvable (RETURN-FIRST-NOT-FALSE_candidateChoice candidateChoice (car lst) (cadr lst) (candidateUpdate candidates
                                                                                                                                                                                                                    (car lst)
                                                                                                                                                                                                                    (cadr lst)
                                                                                                                                                                                                                    treeLocs
                                                                                                                                                                                                                    (caddr lst)
                                                                                                                                                                                                                    '()) 4)
                                                                                                               (candidateUpdate candidates
                                                                                                                                (car lst)
                                                                                                                                (cadr lst)
                                                                                                                                treeLocs
                                                                                                                                (caddr lst)
                                                                                                                                '())
                                                                                                               (car lst)
                                                                                                               (cadr lst)
                                                                                                               treeLocs
                                                                                                               (caddr lst)
                                                                                                               (- totalTentCount (length (caddr lst)))
                                                                                                               totalTentCount
                                                                                                               )
                                                                                                     ))))))




;Returns first not false got given candidateChoice func.
(define (RETURN-FIRST-NOT-FALSE_candidateChoice function rowCounts colCounts candidateList remTent) (if (not (pair? candidateList)) (RETURN-FIRST-NOT-FALSE_candidateChoice function rowCounts colCounts candidateList (+ remTent 1))
                                                                                                        (if (false? (function rowCounts colCounts (car candidateList) remTent  )) (RETURN-FIRST-NOT-FALSE_candidateChoice function rowCounts colCounts (cdr candidateList) remTent )
                                                                                                            (car candidateList))))



;update row and column remaining tent counts
(define (updateRowCounts candidate rowCounts)(REPLACE-NTH rowCounts (car candidate) (- (list-ref rowCounts (- (car candidate) 1)) 1) ))
(define (updatecolCounts candidate colCounts)(REPLACE-NTH colCounts (cadr candidate) (- (list-ref colCounts (- (cadr candidate) 1)) 1) ))



;Candidate updater for next iteration
(define (candidateUpdate candidateList rowCounts colCounts treeLocs tentLocs lastUpdated ) (if (not (pair? (cdr candidateList))) (if (checker (car candidateList) rowCounts colCounts treeLocs tentLocs) (set->list (list->set (append lastUpdated (list (car candidateList))))) (set->list (list->set lastUpdated)))
                                                                                            (if (checker (car candidateList) rowCounts colCounts treeLocs tentLocs) (candidateUpdate (cdr candidateList) rowCounts colCounts treeLocs tentLocs (append lastUpdated (list (car candidateList))))
                                                                                                (candidateUpdate (cdr candidateList) rowCounts colCounts treeLocs tentLocs lastUpdated ))))

;fill obligatory tents after new tent     
(define (fillTents candidates rowCounts colCounts rowInfos colInfos tentLocs) (if (not (pair? candidates)) (list rowCounts colCounts (set->list (list->set tentLocs)))
                                                                                  (if (or (= (list-ref rowCounts (- (caar candidates) 1)) (list-ref rowInfos (- (caar candidates) 1)) ) (= (list-ref colCounts (- (cadar candidates) 1)) (list-ref colInfos (- (cadar candidates) 1)) ))
                                                                                      (fillTents (cdr candidates) (updateRowCounts (car candidates) rowCounts) (updatecolCounts (car candidates) colCounts)  rowInfos colInfos (append tentLocs (list (car candidates))))
                                                                                      (fillTents (cdr candidates) rowCounts colCounts rowInfos colInfos tentLocs) )))

;checks whether all trees have adjacent tent or not
(define (Isfinished treeLocs tentLocs ) (if (not (pair? treeLocs)) #t
                                            (if (ADJACENT-WITH-LIST (car treeLocs)  tentLocs ) (Isfinished (cdr treeLocs) tentLocs)
                                                #f)))


;fill candidate's information into rowInfos colInfos
(define (fillCandidateInfos candidate candidates rowInfos colInfos)(if (not (pair? candidates) ) (list rowInfos colInfos )
                                                 (fillCandidateInfos (car candidates) (cdr candidates) (REPLACE-NTH rowInfos (car candidate) (+ (list-ref rowInfos (- (car candidate) 1)) 1) ) (REPLACE-NTH colInfos (cadr candidate) (+ (list-ref rowInfos (- (cadr candidate) 1)) 1) ) ) ))

;checker for candidates
(define (checker candidate rowCounts colCounts treeLocs tentLocs) (if (> (car candidate) (length rowCounts) ) #f;
                                                                      (if (> (car (cdr candidate)) (length colCounts) ) #f;
                                                                          (if (not (false? (memv candidate treeLocs))) #f
                                                                              (if (not (false? (memv candidate tentLocs))) #f
                                                                                  (if (not (ADJACENT-WITH-LIST candidate  treeLocs)) #f
                                                                                      (if (ADJACENT-WITH-LIST candidate  tentLocs) #f
                                                                                          (if (> 0 (- (car candidate) 1) ) #f 
                                                                                              (if (> 0 (- (cadr candidate) 1) )  #f
                                                                                                  (if (= 0 (list-ref rowCounts (- (car candidate) 1)) ) #f 
                                                                                                      (if (= 0 (list-ref colCounts (- (cadr candidate) 1) ) ) #f

                                                                                                          #t ) ) ) ) ) ) )))))
; finds next candidate to choose
(define (candidateChoice rowCounts colCounts candidate remTent)(if (or (> remTent (list-ref rowCounts (- (car candidate) 1))) (> remTent (list-ref colCounts (- (cadr candidate) 1) ) ) ) candidate
                                                           #f ) )


;finds first candidate tent locations via tree locations
(define (findCandidates treeLocs) (if (not (pair? (cdr treeLocs))) (NEIGHBOR-LIST (car treeLocs) )
                                               (append (NEIGHBOR-LIST (car treeLocs)) (findCandidates (cdr treeLocs)) )))


;(TENTS-SOLUTION '((1) (1) ()))
;output: #f

;(TENTS-SOLUTION '((1 0) (1 0) ((1 1))))
;output: #f

;(TENTS-SOLUTION '((0 0 0 0 0 0 1 0) (0 0 1 0 0 0 0 0) ((6 3))))
;output: multiple solutions ex: ((7 3))

;##this one is beyond the limits

;(TENTS-SOLUTION '((4 3 3 4 2 2 3 3 4 4 3 4 3 3 0) (4 3 1 5 2 5 2 3 3 4 1 4 2 2 4) ((1 6) (1 9) (2 1) (2 8) (2 10) (2 14) (2 15) (3 2) (3 4) (3 6) (3 7) (4 5) (4 13) (4 14) (5 1) (6 6) (6 9) (7 7) (8 1) (8 5) (8 7) (8 9) (8 12) (8 15) (9 3) (9 8) (9 14) (10 2) (10 6) (10 11) (10 12) (10 15) (11 4) (11 5) (11 6) (11 8) (11 11) (11 15) (12 1) (12 10) (13 4) (13 6) (13 11) (13 15) (15 1))))
;output: ((1 1) (1 8) (1 10) (1 15) (2 4) (2 6) (2 13) (3 1) (3 8) (3 10) (4 4) (4 6) (4 12) (4 15) (5 2) (5 9) (6 5) (6 7) (7 9) (7 12) (7 15) (8 2) (8 4) (8 6) (9 9) (9 11) (9 13) (9 15) (10 1) (10 3) (10 5) (10 7) (11 10) (11 12) (11 14) (12 2) (12 4) (12 6) (12 8) (13 10) (13 12) (13 14) (14 1) (14 4) (14 6))

;(TENTS-SOLUTION '((3 1 3 1 2 1 1 0) (3 1 2 0 2 1 1 2) ((1 2) (1 4) (1 7) (2 1) (3 2) (3 5) (3 6) (3 7) (4 2) (4 7) (6 6) (8 1))))
;output: ((1 1) (1 3) (1 8) (2 6) (3 1) (3 3) (3 8) (4 5) (5 2) (5 7) (6 5) (7 1))

;(TENTS-SOLUTION '((4 0 2 0 2 0 4) (4 0 2 0 2 0 4) ((1 2) (1 4) (1 6) (2 1) (2 7) (4 1) (4 7) (6 1) (6 7) (7 2) (7 4) (7 6))))
;output: ((1 1) (1 3) (1 5) (1 7) (3 1) (3 7) (5 1) (5 7) (7 1) (7 3) (7 5) (7 7))

;(TENTS-SOLUTION '((2 2 1 3 1 3 2 2 3 1) (4 1 1 3 1 2 2 1 3 2) ((1 2) (1 8) (2 5) (2 7) (3 8) (4 1) (4 3) (4 8) (5 3) (5 8) (6 6) (6 10) (7 1) (7 4) (7 8) (9 1) (9 7) (9 9) (10 2) (10 10))))
;output: ((1 1) (1 9) (2 4) (2 6) (3 9) (4 2) (4 4) (4 7) (5 9) (6 1) (6 3) (6 5) (7 7) (7 10) (8 1) (8 4) (9 6) (9 8) (9 10) (10 1))

;(TENTS-SOLUTION '((2 2 1 3 2 3 1 4 1 1) (2 3 2 2 2 2 2 1 2 2) ((1 1) (1 5) (1 6) (3 3) (3 5) (3 9) (4 6) (5 7) (5 10) (6 1) (6 3) (6 4) (7 3) (7 6) (7 9) (7 10) (8 1) (8 7) (9 4) (10 4))))
;output: ((1 2) (1 4) (2 6) (2 9) (3 2) (4 5) (4 7) (4 10) (5 1) (5 3) (6 5) (6 7) (6 9) (7 2) (8 4) (8 6) (8 8) (8 10) (9 1) (10 3))

;(TENTS-SOLUTION '((2 1 2 0 2 1 2) (0 3 0 4 0 3 0) ((1 3) (1 5) (3 3) (3 5) (5 3) (5 5) (7 3) (7 5) (2 1) (6 7))))
;output: ((1 4) (1 6) (2 2) (3 4) (3 6) (5 2) (5 4) (6 6) (7 2) (7 4))

;##this one is beyond the limits

;(TENTS-SOLUTION '((8 1 7 2 5 4 3 5 3 3 6 3 4 2 6 2 4 4 4 4) (3 6 2 5 4 5 4 5 1 8 1 6 2 6 2 4 5 2 3 6) ((1 2) (1 7) (1 9) (1 13) (1 18) (2 4) (2 6) (2 7) (2 14) (2 16) (3 11) (3 15) (3 17) (4 4) (4 8) (4 12) (4 13) (4 14) (5 1) (5 9) (5 17) (5 20) (6 2) (6 3) (6 5) (6 8) (6 12) (6 16) (7 15) (7 20) (8 1) (8 5) (8 6) (8 8) (8 9) (8 20) (9 5) (10 3) (10 5) (10 6) (10 12) (10 17) (11 1) (11 3) (11 15) (11 17) (12 5) (12 8) (12 10) (12 15) (12 20) (13 6) (13 12) (13 18) (14 7) (14 9) (14 14) (14 16) (14 20) (15 3) (15 6) (15 16) (16 2) (16 10) (16 12) (16 20) (17 5) (17 12) (17 13) (18 18) (18 20) (19 3) (19 4) (19 10) (19 13) (19 17) (20 4) (20 7) (20 11) (20 13))))
;output: ((1 4) (1 6) (1 8) (1 10) (1 12) (1 14) (1 16) (1 19) (2 2) (3 4) (3 7) (3 10) (3 12) (3 14) (3 16) (3 18) (4 1) (4 20) (5 3) (5 5) (5 8) (5 10) (5 13) (6 1) (6 15) (6 17) (6 20) (7 6) (7 8) (7 12) (8 2) (8 4) (8 10) (8 15) (8 19) (9 6) (9 8) (9 17) (10 2) (10 4) (10 13) (11 6) (11 8) (11 10) (11 16) (11 18) (11 20) (12 1) (12 3) (12 14) (13 5) (13 7) (13 16) (13 19) (14 10) (14 12) (15 2) (15 5) (15 7) (15 14) (15 17) (15 20) (16 9) (16 11) (17 2) (17 6) (17 14) (17 20) (18 4) (18 10) (18 12) (18 17) (19 2) (19 7) (19 14) (19 20) (20 5) (20 10) (20 12) (20 17))

;(TENTS-SOLUTION '((1 3 1 4 1 4 1 3 1 1) (2 3 1 2 1 2 2 2 1 4) ((1 9) (2 5) (3 1) (3 2) (3 3) (3 7) (4 5) (4 7) (4 10) (5 2) (5 9) (6 3) (6 7) (6 9) (7 2) (7 9) (8 8) (9 5) (9 10) (10 2))))
;output: ((1 10) (2 2) (2 4) (2 7) (3 10) (4 1) (4 3) (4 6) (4 8) (5 10) (6 2) (6 4) (6 6) (6 8) (7 10) (8 2) (8 5) (8 7) (9 9) (10 1))

;(TENTS-SOLUTION '((2 0 2 0 2 0 2) (0 2 0 4 0 2 0) ((1 3) (1 5) (3 3) (3 5) (5 3) (5 5) (7 3) (7 5))))
;output: multiple outputs ex: ((1 2) (1 4) (3 2) (3 4) (5 4) (5 6) (7 4) (7 6))

;(TENTS-SOLUTION '((2 1 2 0 2 0 2) (0 3 0 4 0 2 0) ((1 3) (1 5) (3 3) (3 5) (5 3) (5 5) (7 3) (7 5) (2 1))))
;output: multiple outputs ex: ((1 4) (1 6) (2 2) (3 4) (3 6) (5 2) (5 4) (7 2) (7 4))
(print "----")
(TENTS-SOLUTION '((1 0 1 0 1 0 1 0 1 0) (5) ((2 1) (4 1) (6 1) (8 1) (10 1))))
;((9 1) (7 1) (5 1) (3 1) (1 1))


(TENTS-SOLUTION '( (1 3 0 3 1 3 2 3 2 2) (2 3 2 2 1 2 2 2 2 2) ( (1 2) (2 6) (2 7) (2 10) (3 2) (3 4) (4 9) (5 6) (5 7) (6 2) (6 8) (6 10) (7 1) (7 2) (7 4)(7 6)(9 3) (9 9) (10 2) (10 8))))
;((9 8) (10 1) (9 10) (10 3) (8 6) (8 4) (8 2) (6 1) (7 10) (7 8) (6 3) (4 7) (6 6) (5 9) (4 4) (4 2) (2 9) (1 7) (2 5) (2 2))


;(TENTS-SOLUTION '( (5) (0 1 0 1 0 1 0 1 0 1) ((1 1) (1 3) (1 5) (1 7) (1 9))))
;((1 10) (1 8) (1 6) (1 4) (1 2))

(TENTS-SOLUTION '((0 1 0 1 0 1 0 1 0 1)(5) ((1 1) (3 1) (5 1) (7 1) (9 1))))
;((10 1) (8 1) (6 1) (4 1) (2 1))

(TENTS-SOLUTION '( (2 2 2 1 3 1 1 0) (2 2 1 1 2 1 1 2) ((1 3)(1 4)(1 7)(2 2)(2 7)(4 6)(4 8)(5 2)(5 4)(6 4)(6 7)(8 1))))
;((7 1) (5 7) (6 5) (5 3) (5 1) (3 8) (4 5) (2 6) (3 2) (1 8) (2 4) (1 2))


(TENTS-SOLUTION '( (2 2 2 1 8 1 1 0) (2 2 1 1 2 1 1 2) ((1 3)(1 4)(1 7)(2 2)(2 7)(4 6)(4 8)(5 2)(5 4)(6 4)(6 7)(8 1))))
;#f