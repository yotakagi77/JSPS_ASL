module subprogs
implicit none
contains
subroutine rectangular_v_s(a, b, c, v,s)
real(8), intent(in) :: a, b, c
real(8), intent(out) :: v,s
v = a * b * c
s = (a * b + b * c + c * a) * 2
end subroutine rectangular_v_s
end module subprogs

program main
use subprogs
implicit none
real(8) :: x, y, z, v,s
write(*,*) 'input x '
read(*,*) x
write(*,*) 'input y '
read(*,*) y
write(*,*) 'input z '
read(*,*) z
call rectangular_v_s(x, y, z, v,s)
write(*,*) 'vol =' , v
write(*,*) 'surface =' , s
end program main