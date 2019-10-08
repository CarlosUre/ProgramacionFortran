FUNCTION y(x)
IMPLICIT NONE
REAL, INTENT(in):: x
REAL:: Pm, y
REAL,PARAMETER::Qn=1.0
Pm= 1.0+x+(0.5*(x**2.0))
y= Pm/Qn

END FUNCTION y
