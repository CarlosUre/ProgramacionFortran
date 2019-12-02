program taylor

    implicit none                  
    real (kind=8) :: x, y
    real (kind=8), external :: sintaylor
    integer :: n, i,k,pares,a
!Abrimos un un archivo de texto para apuntar los valores de x en la función
    OPEN(unit=2,FILE="senodetaylor.dat")
	DO k=1,6, 1
	a=a+1
	pares=mod(k,2) 
		IF(k==1)THEN
	      n=1
		ELSE
		  n=n+2
		END IF
	 DO i=-100, 100
	  x=0.2*i
	  y=sintaylor(x,n)
	  IF(pares==1.AND.k==1)THEN
	  y=x
	  ELSE iF(pares==1.AND.k>1)THEN
	   y=(-1)*y
	  END IF
	  write(2,*) x,y,a
	 END DO
         write(2,*)
    END DO
close(2)
END PROGRAM
!==========================
function sintaylor(x,n)
!==========================
    implicit none

    ! function arguments:
    real (kind=8), intent(in) :: x
    integer, intent(in) :: n
    real (kind=8) :: sintaylor

    ! local variables:
    real (kind=8) :: term, partial_sum,a,c,b,d
    integer :: j

    partial_sum = 0
    !DO para comenzar la suma desde grado 0 hasta n
    DO j=0,n,1
     a=(-1.0)**j 
     b=2*j+1
	 d=b
     c=x**b 
      DO 
	  d=d-1
	  b=b*d
	  IF(d==1)EXIT
	  IF(d==0)THEN
	  b=1
	  EXIT
	  END IF
     END DO
    term=a/b*c
    partial_sum=partial_sum+term
    END DO
   sintaylor=partial_sum

end function sintaylor

