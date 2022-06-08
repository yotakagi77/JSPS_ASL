program dispersion_relation

implicit none
real :: x0,x1,err, pi, T, h, err0
integer :: i

write(*,'(a,$)') 'input T'
read(*,*) T

write(*,'(a,$)') 'input h'
read(*,*) h

x0 = 2.5
pi = atan(1.0)*4.0
err0 = 10**10

do i = 1, 100
 x1 = T*sqrt(9.8*x0*tanh(2*pi*h/x0)/(2*pi))
 err = abs(x0-x1)
 write(*,*) i,x1, err
 if(err < err0) then
 x0 = x1
 else 
  exit
 endif
enddo

 


end program dispersion_relation
