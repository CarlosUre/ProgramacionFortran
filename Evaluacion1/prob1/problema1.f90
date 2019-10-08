PROGRAM padejoto
IMPLICIT NONE
REAL:: x
INTEGER:: i
REAL, PARAMETER:: pi=3.1415927, dt=0.01
REAL, EXTERNAL:: y

OPEN(11, file="ctm.dat", access="Append")
Do i=0, 10000
x=(-1.0*pi)+(dt*i)
!y(x)
IF(x>=pi) EXIt
Print*, x, y(x)
Write (11,*) x, y(x)
END DO
close(11)
END PROGRAM padejoto

