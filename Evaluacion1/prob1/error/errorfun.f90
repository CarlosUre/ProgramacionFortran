FUNCTION y(x)
IMPLICIT NONE
REAL, INTENT(in):: x
REAL:: Pm, Qn, y
Pm= ((12671.0/4363920.0)*(x**5.0))-((2363.0/18183.0)*(x**3.0))+x
Qn= 1.0+((445.0/12122.0)*(x**2.0))+((601.0/872784.0)*(x**4.0))+((121.0/16662240.0)*(x**6.0))
y= Pm/Qn
END FUNCTION y
