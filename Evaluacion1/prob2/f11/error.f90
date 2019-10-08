PROGRAM padexp
IMPLICIT NONE
REAL:: x, errorrelativo
INTEGER:: i
REAL, PARAMETER:: pi=3.1415927, dt=0.1
REAL, EXTERNAL:: y

OPEN(10, file="ola.dat", access="Append")
Do i=0, 10000
x=(-1.0*10)+(dt*i)
!y(x)
errorrelativo=(exp(x))+((-1.0*y(x))/exp(x))
IF(x>=10) EXIT
Print*, x, errorrelativo
Write (10,*) x, errorrelativo
END DO
close(10)
END PROGRAM padexp
