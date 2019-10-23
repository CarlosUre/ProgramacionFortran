program projectile
  implicit none
			
  ! definimos constantes
  real, parameter :: g = 9.80665
  real, parameter :: pi = 3.1415927
  real, parameter :: dt= 0.1

  ! definimos las variables
  real :: angulo, t, v0, x, y																														
  INTEGER :: n, m

  ! Leer valores para el ángulo a, el tiempo t, y la velocidad inicial u desde la terminal
  write(*,*) 'Dame el tiempo total y la rapidez inicial'
  read(*,*) t, v0
  angulo=45
  OPEN(unit=5,file="salida.dat",access="Append")
  DO m=15, 90, 15
  angulo=float(m)

  ! convirtiendo ángulo a radianes
  angulo = angulo * pi / 180.0
  DO n=0, 100
  t=float(n)*dt
  
  ! las ecuaciones de la posición en x y y
  x = v0 * cos(angulo) * t
  y = v0 * sin(angulo) * t - 0.5 * g * t * t
 
 ! escribiendo el resultado en la pantalla
  write(5,*) x, y
  PRINT*, x, y
  IF(y<=0 .AND. t/=0) EXIT
  END DO
  END DO
  close (5)

end program projectile