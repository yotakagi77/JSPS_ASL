module subprog
implicit none
contains
 subroutine siki(a, t)
 real(8), intent(in) :: a(1:3, 1:3)
 real(8), intent(out) :: t
 real(8)  x, y, z
 x = a(2,2) * a(3,3) - a(2,3) * a(3,2)
 y = a(2,1) * a(3,3) - a(2,3) * a(3,1)
 z = a(2,1) * a(3,2) - a(2,2) * a(3,1)
 t = a(1,1) * x - a(1,2) * y + a(1,3) * z
 write(*,*) t
 end subroutine siki
end module

program ensyu33_2
use subprog
implicit none
real(8) m(3, 3), gyouretusiki
write(*,*) 'input M'
read(*,*) m
call siki(m, gyouretusiki)
end program ensyu33_2