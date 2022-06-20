module subprogs
 implicit none
contains
subroutine solve(a0,x,b,n)
  !ガウスジョルダン法(部分pivot選択あり)
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n+1,n+1), b(n+1)
  real(8), intent(out) :: x(n+1)
  integer i, j, k, m
  real(8) ar, am, t, a(n+1,n+1), d, w(n+1)
  a(:,:) = a0(:,:)
  x(:) = b(:)
  !部分pivot選択
  do k = 1, n+1
   m = k
   am = abs(a(k,k))
   do i = k + 1, n+1
    if(abs(a(i,k)) > am) then
     am = abs(a(i,k))
     m = i
    endif
   enddo
   if(am == 0.0d0) stop 'A is singular'
   if(k /= m) then
    w(k:n+1) = a(k,k:n+1)
    a(k,k:n+1) = a(m,k:n+1)
    a(m,k:n+1) = w(k:n+1)
    t = x(k)
    x(k) = x(m)
    x(m) = t
   endif
   !以下は通常のガウスジョルダン法の演算
   ar = 1.0d0 / a(k,k)
   a(k,k) = 1.0d0
   a(k,k+1:n+1) = ar * a(k,k+1:n+1)
   x(k) = ar * x(k)
   do i = 1, n+1
    if(i/=k) then
      a(i,k+1:n+1) = a(i,k+1:n+1) - a(i,k) * a(k,k+1:n+1)
      x(i) = x(i) - a(i,k) * x(k)
      a(i,k) = 0.0d0
    endif
   enddo
  enddo
  !後退代入
  do i = n, 1, -1
      do j = i + 1, n+1
        x(i) = x(i) - a(i, j) * x(j)
      end do
  end do
  end subroutine solve

  subroutine squares(x,y,m,n,c,b)
   integer, intent(in) :: m, n
   integer :: i, j, k, l
   real(8), intent(in) :: x(m), y(m)
   real(8), intent(out) :: c(n+1,n+1), b(n+1)
   c(:,:) = 0
   b(:) = 0
   do k = 1, n+1
    do j = 1, n+1
     do i = 1, m
      c(k,j) = c(k,j) + x(i) ** (k + j - 2)
     enddo
    enddo
    do l = 1, m
     b(k) = b(k) + y(l) * x(l) ** (k-1)
    enddo
   enddo
   do i = 1, n+1
    write(*,*) (c(i,j), j = 1, n+1)
   enddo
   do i = 1, n+1
    write(*,*) b(i)
   enddo
  end subroutine squares


end module subprogs

  program main
   use subprogs
   implicit none
   integer m, n, k, i, j
   real(8), allocatable :: x(:), y(:), a(:), b(:), c(:,:), d(:)
   m = 10
   n = 3
   allocate (x(m), y(m), a(n+1), b(n+1), c(n+1,n+1), d(n+1))
   open(1,file = 'output6_09.d',status='old')    !データの読み込み
   do i = 1, m
    read(1,*) x(i), y(i)
    write(*,*) x(i), y(i)
   enddo
   close(1)
   call squares(x,y,m,n,c,b)
   call solve(c,a,b,n)
   do k=1, n+1 !次数が低い順で出力されるので、高次順に入れ替える
         d(k) = a(n-k+2)
   end do
   write(*,*) 'coefficient of original function'
   write(*,*) '0.1'
   write(*,*) '0.2'
   write(*,*) '0.5'
   write(*,*) '1.0'
   write(*,*) 'coefficient of obtained function '
   do i = 1, n+1
   write(*,*) d(i)
   enddo
  end program main
   
