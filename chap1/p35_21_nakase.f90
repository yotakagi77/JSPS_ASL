program ensyu21
implicit none
integer :: i, n
double precision :: theta,area,pi
open(1, file = 'output1_21.d')
pi=asin(1.0d+0)*2.0d+0
write(* , *)' input n ( n > 3)'
read(* , *) n
if (n < 3) stop 'stop, n < 3'
do i=3,n
theta=2.0d+0*pi/real(i)
area=real(i)/2.0d+0*sin(theta)
write(1 , *) i,pi-area
enddo
close(1)
end program ensyu21