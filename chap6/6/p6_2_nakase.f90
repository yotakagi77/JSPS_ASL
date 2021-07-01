!1,2,3,4と5,6の解が　-1, 2
module subprog
 implicit none
contains
 subroutine gauss_jordan_pv(a0, x, b, n, m, l)
!a0が元の行列a、bが元の行列b、pがbの要素数
 integer, intent(in) :: n, m, l
 real(8), intent(in) :: a0(n , l), b(m)
 real(8), intent(out) :: x(n)
 integer i, k, s
 real(8) ar, am, t
 real(8), allocatable :: a(:, :), w(:)
 allocate(a(n, l), w(l))
 a(:, :) = a0(:, :)
 x(:) = b(:)
 do k = 1, n
   !----部分pivot選択---
   am = 0.0d0
   s=k
   am = abs(a(k, k))
   do i = k, n
     !絶対値が最大のｍ行を探す
     if (abs(a(i, k)) > am) then
       am = abs(a(i, k))
       s = i
       write(*,*) 'test1'
     end if
   end do
   if (am == 0.0d0) stop 'A is singular'
   !kが最大の行でなければ最大の行と入れ替える
   if (k /= s) then
     write(*,*) 'test2'
     w(k:l) = a(k, k:l)
     write(*,*) 'testA'
     a(k, k:l) = a(s, k:l)
     write(*,*) 'testB'
     a(s, k:l) = w(k:l)
     write(*,*) 'test3'
     t = x(k)
     x(k) = x(s)
     x(s) = t
     write(*,*) 'test4'
   end if
   do i = 1, n
     write(*,*) a(i, 1:l)
   end do
   !----以下はガウスジョルダン。まずは対角要素が1になるようにする。
   ar = 1.0d0 / a(k, k)
   a(k, k) =1.0d0
   a(k, k+1:l) =ar * a(k, k+1:l) !対角要素を1にする係数
   x(k) = ar * x(k)
   do i = 1, n
     if (i /= k) then !対照の行以外で引き算をして同じ列の要素を0にする
       a(i, k+1:l) = a(i, k+1:l) - a(i, k) * a(k, k+1:l)
       x(i) = x(i) -a(i, k) * x(k)
       a(i, k) = 0.0d0
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
real(8), allocatable :: a(:, :), a0(:, :), b(:), x(:), r(:)
integer n, m, i, l
write(*,*) 'Input n and m'
read(*,*) n, m
l = n+m !lは拡張行列の列の数
allocate(a(n, n), a0(n, l), b(m), x(n))
write(*,*) ' input a'
read(*,*) a
write(*,*) ' input b'
read(*,*) b
write(*,*) ' '
!拡張行列を作る
 a0(:, :) = a(:, :)
 a0(1:n, n+1:l) = 0.0d0
 do i = 1, m
  a0(i, n+i) = b(i)
 end do
 !いったん出力
  do i = 1, n
   write(*,*) a0(i, 1:l)
  end do
  write(*,*)' '
call gauss_jordan_pv(a0, x, b, n, m, l)
allocate(r(n))
r(:) = b(:) - matmul(a,x)
write(*,*) 'Gauss-Jordan error = ', dot_product(r, r)
end program ensyu6
