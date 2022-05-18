module subprogs
implicit none
contains
subroutine rectangular_vol(a, b, c, v)
real(8), intent(in) :: a, b, c
real(8), intent(out) :: v
v = a * b * c
end subroutine rectangular_vol
end module subprogs

program main
use subprogs
implicit none
real(8) :: x, y, z, vol
write(*,*) 'input x '
read(*,*) x
write(*,*) 'input y '
read(*,*) y
write(*,*) 'input z '
read(*,*) z
call rectangular_vol(x, y, z, vol)
write(*,*) 'vol =' , vol
end program main