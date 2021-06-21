module subprog
 implicit none
contains
 subroutine gauss_jordan_pv(a0, x, i, n)
 integer, intent(in) :: n
 real(8), intent(in) :: a0(n,n), i(n,n)
 real(8), intent(out) :: x(n,n)
 integer j, k, m
 real(8) ar, am, t(n), a(n,n), w(n)
 a(:,:) = a0(:,:)
 x(:,:) = i(:,:)
 do k = 1,n
!部分pivot選択
  m=k
  am = abs(a(k, k))
  do j = k+1, n
   if (abs(a(j, k)) > am) then
    am = abs(a(j, k))
    m = j
   endif
  end do
  if (am == 0.0d0) stop 'A is singular'
  if(k /= m) then
   w(k:n) = a(k, k:n)
   a(k, k:n) = a(m, k:n)
   a(m, k:n) = w(k:n)
   t(1:n) = x(k, 1:n)
   x(k, 1:n) = x(m, 1:n)
   x(m, 1:n) = t(1:n)
  endif

!以下はガウスジョルダン
  ar = 1.0d0 / a(k, k)
  a(k, k) =1.0d0
  a(k, k+1:n) = ar * a(k, k+1:n)
  x(k, 1:n) = ar * x(k, 1:n)
  do j = 1, n
   if (j /= k) then
    a(j, k+1:n) = a(j, k+1:n) - a(j, k) * a(k, k+1:n)
    x(j, 1:n) = x(j, 1:n) -a(j, k) * x(k, 1:n)
    a(j, k) = 0.0d0
   endif
  end do
 end do
 do j = 1, n
  write(*,*) (x(j,k),k=1,n)
 end do
 end subroutine gauss_jordan_pv
end module subprog

program ensyu3
use subprog
implicit none
real(8), allocatable ::  a(:,:), i(:,:), x(:,:)
integer j,n
write(*,*) 'Input n'
read(*,*) n
allocate(a(n,n), i(n,n), x(n,n))
write(*,*) ' input a'
read(*,*) a
i(:,:)=0.0d0
do j=1,n
 i(j,j)=1.0d0
end do
call gauss_jordan_pv(a, x, i, n)
deallocate(a, i, x)
end program ensyu3