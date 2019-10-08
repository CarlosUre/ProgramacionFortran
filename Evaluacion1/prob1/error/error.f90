PROGRAM padejoto
IMPLICIT NONE
REAL:: x, error
INTEGER:: i
REAL, PARAMETER:: pi=3.1415927, dt=0.01
REAL, EXTERNAL:: y

OPEN(5, file="error.dat", access="Append")
Do i=0, 10000
x=float(i)*dt
!y(x)
error= (sin(x))+((-1.0*y(x))/sin(x))
IF(x>=pi) EXIT
Print*, x, error
Write (11,*) x, error
END DO
close(11)
END PROGRAM padejoto

