module subprog
 implicit none
contains
 function sum(a) result(u0)
 real(8), intent(in) :: a(:)
 real(8) s0, u0
 integer i
 write(*,*) size(a, 1) 
 s0 = 0.0d0
 do i = 1, size(a, 1)
  s0 = s0 + a(i)**2 
 end do
  u0 = sqrt(s0)
 end function sum
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
ul = sum(u)
write(*,*) 'ul ', ul
end program ensyu18