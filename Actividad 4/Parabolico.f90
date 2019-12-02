PROGRAM airjordan
 IMPLICIT NONE
	!definir constantes
	REAL,PARAMETER:: g= -9.81
	REAL,PARAMETER:: pi= 3.1415927
	REAL,PARAMETER:: dt= 0.01
	!definir variables
	REAL:: m, v0, vt, k, y, x, t, vx0, vy0, angulo, vx, vy
	!Con aire
	REAL:: yv,rn,rv,xmax,ymax,tymax,txmax,vxmax,vymax,vsuelo
	!Sin aire
	REAL:: sxmax,stxmax,symax,stymax,svxmax,svymax,svsuelo
	INTEGER:: n, i
	!Leer valores para resolver problema
	PRINT*, "Dame la masa del objeto y la velocidad inicial"
	READ*, m, v0
	PRINT*, "Ahora dame la velocidad terminal"
	READ*, vt
	!convertir el angulo a radianes
	yv=0
	rv=1
	angulo = 45.0
	angulo=angulo*pi/180.0
	!Obtener las componentes de la velocidad inicial
	vx0= v0 * cos(angulo)
	vy0= v0 * sin(angulo)
	!Constante de viscocidad del aire
	k=(m*g)/vt
	!Open para guardar los datos
	OPEN(unit=1,file="michael.dat",access="Append")
	!ciclo para el tiempo
	Print*,"calcularemos el tiro con friccion de aire"
	DO n=0, 9999
	t=float(n)*dt
	!velocidad en el eje x
	vx= vx0*exp((-k*t)/m)
	!velocidad en el eje y
	vy= (vy0-((m*g)/k))*exp((-k*t)/m)+(m*g)/k
	!ecuaciones de la posición en x y y
	y = ((m/k)*(vy-((m*g)/k))*(1-(exp((-k/m)*t)))+((m*g)/k)*t)*3
	IF(y<=0 .AND. t/=0) EXIT
	x = (m/k)*vx*(1-exp((-k*t)/m))*3
	!Buscamos la ymax por una diferencia de desplazamientos
	rn=y-yv
	IF(rn<0.AND.rv>0)THEN
	ymax=yv
	tymax=t
	END IF
	rv=rn
	yv=y
	!Escribiendo resultados a pantalla

	PRINT*, x, y
	WRITE(1,*) x, y
	END DO
	xmax=x
	txmax=t
	!Velocidad en xmax
	vxmax= vx0*exp((-k*txmax)/m)
	!Velocidad en ymax
	vymax= (vy0-((m*g)/k))*exp((-k*txmax)/m)+(m*g)/k
	vsuelo= sqrt(vxmax**2+vymax**2)
	WRITE(*,*)"Alcanza horizontal maximo",xmax,"en el tiempo:",txmax
	WRITE(*,*)"Alcance verticual maximo",ymax,"en el tiempo:",tymax
	WRITE(*,*)"Magnitud de la velocidad al tocar el suelo",vsuelo
	!Escribimos un espacio en blanco para que no se junten los resultados
	WRITE(1,*) 
	!Le damos valores de 0 a x y y para poder trabajar sin resistencia del aire
	x=0
	y=0
	t=0
	
	Print*,"Ahora calcularemos el tiro sin frinccion al aire"
	DO i=0, 9999
	t=float(i)*dt
	! las ecuaciones de la posición en x y y
    x = v0 * cos(angulo) * t
    y = v0 * sin(angulo) * t - 0.5 * (-1) * g * t * t
	!Escribiendo los resultados en el archivo
	write(1,*) x, y
	IF(y<=0 .AND. t/=0) EXIT
	END DO
	stxmax=t
	sxmax=x
	stymax=stxmax/2
	symax = v0 * sin(angulo) * (stymax) - 0.5 * (-1) * g * (stymax)**2
	svxmax=v0*cos(angulo)
	svymax=v0*sin(angulo)+g*stxmax
	svsuelo= sqrt(svxmax**2+svymax**2)

	WRITE(*,*)"El alcance horizontal maximo es",sxmax,"en un tiempo:",stxmax
	WRITE(*,*)"El alcance vertical maximo es",symax,"en un tiempo:",stymax
	WRITE(*,*)"La velocidad al impactar en el suelo es:",svsuelo
	close(1)

END PROGRAM airjordan