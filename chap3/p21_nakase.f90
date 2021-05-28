!1,2,3 と 4,5,6で-3,6,3
module subprog
 implicit none
contains
 subroutine gyouretusiki(x, siki)
 real(8), intent(in) :: x(2, 2)
 real(8), intent(out) :: siki
 siki = x(1, 1) * x(2, 2) - x(1, 2) * x(2, 1)
 write(*, *) siki
 end subroutine
end module

program ensyu21
use subprog
implicit none
real(8) a( 3 ), b( 3 ), c(2, 3), x, y, z,xg(2, 2), yg(2, 2), zg(2, 2)
integer i
write(*,*) 'inut a,b'
read(*,*) a, b
!a,bをcに格納
c(1, 1:3) =a (1:3)
c(2, 1:3) = b(1 :3)
do i = 1, 2
 write( * , *) 'c(:)= ', c( i, 1:3)
end do
xg(:, :) = c(1:2, 2:3)
yg(:, :) = c(1:2 , 3:1:-2)
zg(:, :) = c(1:2, 1:2)
do i = 1, 2
 write( * , *) 'xg(:)= ', xg( i, 1:2)
end do
do i = 1, 2
 write( * , *) 'yg(:)= ', yg( i, 1:2)
end do
do i = 1, 2
 write( * , *) 'zg(:)= ', zg( i, 1:2)
end do
write(* ,*) ' x '
call gyouretusiki(xg, x)
write(* ,*) ' y '
call gyouretusiki(yg, y)
write(* ,*) ' z '
call gyouretusiki(zg, z)
end program ensyu21