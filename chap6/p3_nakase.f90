!3,2,1,1 と 0,4 で -4, 12
!3,0,1,0,1,0,0,0,2と1,0,2で0.33,0,0.83
module subprog
 implicit none
contains
 subroutine gauss_jordan_pv(a0, x, b, n)
 integer, intent(in) :: n
 real(8), intent(in) :: a0(n , n), b(n)
 real(8), intent(out) :: x(n)
 integer i, k, m
 real(8) ar, am, t, a(n, n), w(n)
 a(:, :) = a0(:, :)
 x(:) = b(:)
 do k = 1, n
!部分pivot選択
  m=k
  am = abs(a(k, k))
  do i = k+1, n
   if (abs(a(i, k)) > am) then
    am = abs(a(i, k))
    m = i
   endif
  end do
  if (am == 0.0d0) stop 'A is singular'
  if(k /= m) then
   w(k:n) = a(k, k:n)
   a(k, k:n) = a(m, k:n)
   a(m, k:n) = w(k:n)
   t  = x(k)
   x(k) = x(m)
   x(m) = t
  endif

!以下はガウスジョルダン
  ar = 1.0d0 / a(k, k)
  a(k, k) =1.0d0
  a(k, k+1:n) =ar * a(k, k+1:n)
  x(k) = ar * x(k)
  do i = 1, n
   if (i /= k) then
    a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
    x(i) = x(i) -a(i, k) * x(k)
    a(i, k   ) = 0.0d0
   endif
  end do
 end do
 do i = 1, n
  write(*,*) x(i)
 end do
 end subroutine gauss_jordan_pv
end module subprog

program ensyu3
use subprog
implicit none
real(8), allocatable ::  a(:, :), b(:), x(:), c(:, :)
integer n
write(*,*) 'Input n'
read(*,*) n
allocate(a(n, n), b(n), x(n), )
write(*,*) ' input a'
read(*,*) a
write(*,*) 'input b'
read(*,*) b
call random_number(x(:))
call gauss_jordan_pv(a, x, b, n)
deallocate(a, b, x)
end program ensyu3