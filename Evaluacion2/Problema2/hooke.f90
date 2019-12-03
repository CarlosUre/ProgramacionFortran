PROGRAM ONE_D_MOTION
!
! Program for the motion of a particle subject to an external
! force f(x) = -x.   We have divided the total time 2*pi into
! 10000 intervals with an equal time step.   The position and
! velocity of the particle are written out at every 500 steps.
! Copyright (c) Tao Pang 1997.
!
  IMPLICIT NONE
  INTEGER, PARAMETER :: N=10001,IN=500
  INTEGER :: i
  REAL :: pi,DT,k
  REAL, DIMENSION (N):: T,V,X
! Assign constants, initial position, and initial velocity
PRINT*,"Dame el valor de la k"
READ*, k
  pi   = 4.0*ATAN(1.0)
  DT   = 2.0*pi/FLOAT(N-1)
  X(1) = 0.0
  T(1) = 0.0
  V(1) = 1.0
!
! Recursion for position and velocity at later time
 OPEN(Unit=15, file="salida.dat")
  DO i = 1, N-1
    T(i+1) = DT*i
    X(i+1) = X(i)+V(i)*DT
    V(i+1) = V(i)-k*X(i)*DT
  END DO
!
! Write the position and velocity every 500 steps
!
  WRITE (15,"(3F16.8)") (T(i),X(i),V(i),i=1,N,IN)
close(15)
END PROGRAM ONE_D_MOTION
