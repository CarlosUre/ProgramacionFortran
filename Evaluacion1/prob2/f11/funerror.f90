FUNCTION y(x)
IMPLICIT NONE
REAL, INTENT(in):: x
REAL:: Qn, y, Pm
Pm= 1.0+(0.5*x)
Qn=1.0-(0.5*x)
y= Pm/Qn

END FUNCTION y
