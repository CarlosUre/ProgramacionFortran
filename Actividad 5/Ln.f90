PROGRAM	logaritmo
IMPLICIT NONE
REAL,external::ln
INTEGER::i,g,n
REAL::x,y

OPEN(unit=1,file="ln.dat",access="Append")
DO i=1,4,1
	IF(i==1)THEN
	g=4
	else if(i==2)then
	g=7
	else if(i==3)then
	g=11
	else if(i==4)then
	g=16
	END IF

	DO n=-1000,1000,1
         x=0.01*n      
         y=ln(x,g) 
          WRITE(1,*) x,y 
    END DO
        WRITE(1,*) ' '  
    END DO
   CLOSE (1)
END PROGRAM


FUNCTION ln(x,g)
IMPLICIT NONE
!
REAL,intent(in)::x
INTEGER,intent(in)::g
REAL::ln
REAL::termino,suma,a,b,c
INTEGER:: m

suma=0
DO m=1,g,1 
     a=(-1.0)**(m+1)
     b=x**m   
     c=m        

      termino=a*(b/c)     
       termino=suma+termino   
   END DO

    Ln=suma 

END FUNCTION Ln





