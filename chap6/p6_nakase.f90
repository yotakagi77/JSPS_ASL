module subprog
 implicit none
contains
 subroutine gauss_jordan_pv(a0, x, b, n, p, l)
!a0が元の行列a、bが元の行列b、pがbの要素数
 integer, intent(in) :: n, p, l
 real(8), intent(in) :: a0(n , n), b(p)
 real(8), intent(out) :: x(n)
 integer i, k, m
 real(8) ar, am, t, a(n, l), w(n)
 x(:) = b(:)
!拡張行列を作る
 a(1:n, 1:n) = a0(1:n, 1:n)
 a(1:n, n+1:l) = 0.0d0
 do i = 1, p
  a(i, n+i) = b(i)
 end do
!いったん出力
 do i = 1, n
  write(*,*) a(i, 1:l)
 end do

 do k = 1, n
!部分pivot選択
  m=k
  am = abs(a(k, k))
  do i = k+1, n
!絶対値が最大のｍ行を探す
   if (abs(a(i, k)) > am) then
    am = abs(a(i, k))
    m = i
   end if
  end do
  if (am == 0.0d0) stop 'A is singular'
  if (k /= m) then
   w(k:l) = a(k, k:l)
   a(k, k:l) = a(m, k:l)
   a(m, k:l) = w(k:l)
   t  = x(k)
   x(k) = x(m)
   x(m) = t
  end if
!以下はガウスジョルダン。まずは対角要素が1になるようにする。
  ar = 1.0d0 / a(k, k)
  a(k, k) =1.0d0
  a(k, k+1:l) =ar * a(k, k+1:l)
  x(k) = ar * x(k)
  do i = 1, n
   if (i /= k) then
    a(i, k+1:l) = a(i, k+1:l) - a(i, k) * a(k, k+1:l)
    x(i) = x(i) -a(i, k) * x(k)
    a(i, k   ) = 0.0d0
   endif
  end do
 end do

!出力
 do i = 1, n
  write(*,*) a(i, 1:l)
 end do
 write(*,*)' '
 do i = 1, n
  write(*,*) x(i)
 end do
 end subroutine gauss_jordan_pv
end module subprog

program ensyu6
use subprog
implicit none
real(8), allocatable :: a(:, :), b(:), x(:)
integer n, m, i, t
write(*,*) 'Input n and m'
read(*,*) n, m
t = n+m
allocate(a(n, n), b(m), x(n))
write(*,*) ' input a'
read(*,*) a
write(*,*) ' input b'
read(*,*) b
write(*,*) ' '
call random_number(x(:))
call gauss_jordan_pv(a, x, b, n, m, t)
end program ensyu6