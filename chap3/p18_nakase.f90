module subprog
 implicit none
contains
 subroutine wa(a, u0)
 real(8), intent(out) ::  u0
 real(8), intent(in) :: a(:)
 real(8) s0
 integer i
 write(*,*) size(a, 1) 
 s0 = 0.0d0
 do i = 1, size(a, 1)
  s0 = s0 + a(i)**2 
 end do
  u0 = sqrt(s0)
 write(*,*) 'ul= ', u0
 end subroutine wa
end module subprog

program ensyu18
use subprog
implicit none
real(8) , allocatable :: u( : )
real(8) ul
integer n
write(*,*) 'input n'
read(*,*) n
allocate(u(n))
write(*,*) 'input u'
read(*,*) u(:)
write(*,*) 'u(1:n)= ', u(:)

call wa(u(:), ul)
end program ensyu18