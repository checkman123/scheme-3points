;Define Pi
(define pi 3.14159265358)

;Convert Radian to Degree
(define (convert-R-D num)
    (round-five-decimal (/ (* num 180) pi)))

;3 Decimal Precision
(define (round-three-decimal num)
  (let ((x (expt 10.0 3)))
    (/ (floor (* x num)) x)))

;4 Decimal Precision
(define (round-four-decimal num)
  (let ((x (expt 10.0 4)))
    (/ (floor (* x num)) x)))

;5 Decimal Precision
(define (round-five-decimal num)
  (let ((x (expt 10.0 5)))
    (/ (round (* x num)) x)))

;Make a point
(define (make-point x-cor y-cor) (cons x-cor y-cor))

;Get X
(define (get-x point) (car point))

;Get Y
(define (get-y point) (cdr point))

;Get Slope ((Y2-Y1)/(X2-X1))
(define (get-slope x1 y1 x2 y2) 
    (/ (- y2 y1) (- x2 x1))
)

;Using the created points (using make-point) this function returns true (#t) if the three points form a line or false (#f) if they form a triangle.
(define (is-line p1 p2 p3)
    (if (or (= 0 (- (get-x p3)(get-x p1))) (= 0 (- (get-x p2)(get-x p1))))
        #t
        (if (=  (get-slope (get-x p1) (get-y p1) (get-x p2) (get-y p2))
                (get-slope (get-x p1) (get-y p1) (get-x p3) (get-y p3))) 
            
            #t 
            #f)))

;Calculate Distance 
(define (get-dist x1 y1 x2 y2) 
    (sqrt(+ (expt (- x2 x1) 2) 
            (expt (- y2 y1) 2))))

;Calculates the distance between two points
(define (distance p1 p2)
    (get-dist (get-x p1) (get-y p1) (get-x p2) (get-y p2))
)

;Calculates the perimeter of a triangle defined by the three points if it isn't a line
(define (perimeter p1 p2 p3)
    (if (equal? #f (is-line p1 p2 p3))
        (+ (+ (distance p1 p2) (distance p2 p3)) 
            (distance p1 p3))
        (display "Error: The 3 points form a line")))

;Calculate S, S-A, S-B, S-C
(define (find-s s1 s2 s3) 
    (/  (+  (+ s1 s2)
            s3) 
        2))

;Calculate S-Side A,B,C
(define (find-sa s1 s2 s3) 
    (-  (/  (+  (+ s1 s2)
            s3) 
        2)
        s1))

(define (find-sb s1 s2 s3) 
    (-  (/  (+  (+ s1 s2)
            s3) 
        2)
        s2))

(define (find-sc s1 s2 s3) 
    (-  (/  (+  (+ s1 s2)
            s3) 
        2)
        s3))

;Calculates the area of a triangle defined by the three points using Heron's formula sqrt(S(S-A)(S-B)(S-C))
(define (area p1 p2 p3)
    (sqrt   (*  (*  (*  (find-s (distance p1 p2) (distance p2 p3) (distance p1 p3))
                                    (find-sa (distance p1 p2) (distance p2 p3) (distance p1 p3)))
                                (find-sb (distance p1 p2) (distance p2 p3) (distance p1 p3)))
                            (find-sc (distance p1 p2) (distance p2 p3) (distance p1 p3)))))

;Calculate angle 1
(define (find-angle1 p1 p2 p3)
  (acos (/ (- (+ (expt (distance p2 p3) 2) (expt (distance p1 p3) 2)) (expt (distance p1 p2) 2)) (* (* (distance p2 p3) (distance p1 p3)) 2 ))))

;Calculate angle 2
(define (find-angle2 p1 p2 p3)
  (acos (/ (- (+ (expt (distance p1 p3) 2) (expt (distance p1 p2) 2)) (expt (distance p2 p3) 2)) (* (* (distance p1 p3) (distance p1 p2)) 2 ))))

;Calculate angle 3
(define (find-angle3 p1 p2 p3)
  (acos (/ (- (+ (expt (distance p1 p2) 2) (expt (distance p2 p3) 2)) (expt (distance p1 p3) 2)) (* (* (distance p1 p2) (distance p2 p3)) 2 ))))


;Calculates the perimeter, area and interior angles of the triangle formed by the three points
(define (calculate-triangle p1 p2 p3)
    (display "Side 1 = ")
    (display (round-four-decimal (distance p1 p2)))
    (newline)

    (display "Side 2 = ")
    (display (round-four-decimal (distance p2 p3)))
    (newline)

    (display "Side 3 = ")
    (display (round-four-decimal (distance p1 p3)))
    (newline)

    (display "Perimeter = ")
    (display (round-three-decimal (perimeter p1 p2 p3)))
    (newline)

    (display "Area = ")
    (display (round (area p1 p2 p3)))
    (newline)

    (display "Angle 1 = ")
    (display (round-five-decimal (find-angle1 p1 p2 p3)))
    (display "  ")
    (display (convert-R-D (find-angle1 p1 p2 p3)))
    (newline)

    (display "Angle 2 = ")
    (display (round-five-decimal (find-angle2 p1 p2 p3)))
    (display "  ")
    (display (convert-R-D (find-angle2 p1 p2 p3)))
    (newline)

    (display "Angle 3 = ")
    (display (round-five-decimal (find-angle3 p1 p2 p3)))
    (display "  ")
    (display (convert-R-D (find-angle3 p1 p2 p3)))
    (newline))