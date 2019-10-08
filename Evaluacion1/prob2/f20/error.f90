PROGRAM padexp
IMPLICIT NONE
REAL:: x, errorrelativo
INTEGER:: i
REAL, PARAMETER:: pi=3.1415927, dt=0.01
REAL, EXTERNAL:: y

OPEN(5, file="lloranding.dat", access="Append")
Do i=0, 10000
x=(-1.0*10)+(dt*i)
!y(x)
errorrelativo=(exp(x))+((-1.0*y(x))/exp(x))
IF(x>=10) EXIT
Print*, x, errorrelativo
Write (5,*) x, errorrelativo
END DO
close(5)
END PROGRAM padexp
