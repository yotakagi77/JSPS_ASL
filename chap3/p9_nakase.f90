module subprog
 implicit none
contains
 function sekibun(a, b) result(P)
  integer  i,n
  real(8), intent(in) ::  a,b
  real(8) h,y1,y2, pi, P
  pi = 2.0d0 * acos(0.0d0)
!分割数
  write(*,*) 'Input n(Division number)'
  read(*,*) n
  h=(b-a)/n
!台形則
  P=0
  do i=1,n
   y1 = exp(- 0.5d0 * (a + (i-1) * h)**2) / sqrt(2*pi)
   y2 = exp(- 0.5d0 * (a+ i * h)**2) / sqrt(2*pi)
   P=P + 0.5d0 * (y1 + y2) * h
  end do
 end function sekibun
end module subprog

program ensyu9
use subprog
implicit none
real(8) a, b, p
write(*,*) "Integral range of normal distribution [a,b]"
write(*,*) " Input a,b (a<b) "
read(*,*) a,b
p = sekibun(a, b)
write(*,*) p
end program ensyu9
