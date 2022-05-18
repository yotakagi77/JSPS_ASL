module subprogs
implicit none
contains
function func_ellip(n,k) result(k_n)
 integer,intent(in) :: n
 integer i 
 real(8),intent(in) :: k
 real(8) pi, a_n, k_n
 pi = 2.0d0 * acos(0.0d0)
 k_n = pi / 2
 a_n = 1.0d0
  do i = 1, n-1 
   a_n = a_n * ((2*i-1)**2) / ((2*i)**2)
   k_n = k_n + (pi / 2) * a_n * (k**(2*i))
  end do
 end function func_ellip

end module subprogs

program main
use subprogs
implicit none
integer n
real(8) k 
write(*,*) 'input n'
read(*,*) n
if(n < 1) then
write(*,*) 'input integer n >1 '
stop
endif
write(*,*) 'input k(0 < k^2 < 1)'
read(*,*) k
if(k**2 >1) then
write(*,*) 'sorry, input k(0 < k^2 < 1)'
stop
endif
write(*,*) 'K(k) ~ ', func_ellip(n,k)

end program main
