program ensyu21
implicit none
integer :: i, n
double precision :: theta,area,pi
open(1, file='output1_21.d')
pi=2.0d0 * acos(0.0d0)
write(* , *)' input n ( n > 3)'
read(* , *) n
if (n < 3) stop 'stop, n < 3'
do i = 3, n
 theta = 2*pi / i
 area = i * 0.5d0 * 1 * 1 * sin(theta)
 write(1 , *) i, pi-area
end do
close(1)
end program ensyu21