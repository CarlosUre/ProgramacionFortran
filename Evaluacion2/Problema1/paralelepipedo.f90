PROGRAM Triangle
     IMPLICIT NONE
     REAL :: a, b, c
     INTEGER:: n
!b= base
!height=altura

     REAL, EXTERNAL:: Area
     REAL, EXTERNAL:: Volumen
     PRINT *, 'Welcome, please enter the&
              &lengths of the 3 sides.'
     READ *, a, b, c
     PRINT *, "Triangle''s area: ", Area(a,b,c)
     PRINT*, "si tu figura es un paralelepipedo, te gustaria saber su volumen? yes=1 no=2"
     READ*, n
     IF(n==1) then
     PRINT*, Volumen(a,b,c)
     END IF
     IF(n==2) THEN
     END IF
    END PROGRAM Triangle
    FUNCTION Area(a,b,c)
     IMPLICIT NONE
     REAL :: Area            ! function type
     REAL, INTENT( IN ) :: a, b, c
     REAL :: theta, height
     theta = b*COS(((b**2+a**2-c**2)/(2.0*b*a)))
     height = b*SIN(theta); Area = 0.5*a*height
    END FUNCTION Area
FUNCTION Volumen(a,b,c)
REAL, INTENT(IN):: a,b,c
REAL:: Volumen
Volumen= a*b*c
END FUNCTION Volumen
