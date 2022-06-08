module subprogs
 implicit none
contains
 subroutine solve(a0,x,b,n)
  !ガウスジョルダン法(部分pivot選択あり)
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n+1,n+1), b(n+1)
  real(8), intent(out) :: x(n+1)
  integer i, j, k, m
  real(8) ar, am, t, a1(n+1,n+1), d, w(n+1)
  a1(:,:) = a0(:,:)
  x(:) = b(:)
  !部分pivot選択
  do k = 1, n + 1
   m = k
   am = abs(a1(k,k))
   do i = k + 1, n + 1
    if(abs(a1(i,k)) > am) then
     am = abs(a1(i,k))
     m = i
    endif
   enddo
   if(am == 0.0d0) stop 'A is singular'
   if(k /= m) then
    w(k:n+1) = a1(k,k:n+1)
    a1(k,k:n+1) = a1(m,k:n+1)
    a1(m,k:n+1) = w(k:n+1)
    t = x(k)
    x(k) = x(m)
    x(m) = t
   endif
   !以下は通常のガウスジョルダン法の演算
   ar = 1.0d0 / a1(k,k)
   a1(k,k) = 1.0d0
   a1(k,k+1:n+1) = ar * a1(k,k+1:n+1)
   x(k) = ar * x(k)
   do i = 1, n+1
    if(i/=k) then
      a1(i,k+1:n+1) = a1(i,k+1:n+1) - a1(i,k) * a1(k,k+1:n+1)
      x(i) = x(i) - a1(i,k) * x(k)
      a1(i,k) = 0.0d0
    endif
   enddo
  enddo
  !後退代入
  do i = n, 1, -1
      do j = i + 1, n + 1
        x(i) = x(i) - a1(i, j) * x(j)
      end do
  enddo
  end subroutine solve

  subroutine squares(x,y,b,c,m,n)
   !------最小2乗法-------
   integer m, n, k, i, j
   real(8) x(m), y(m), b(n+1), c(n+1,n+1), s, t
   s = 0
   t = 0
   do k = 1, n+1
    do j = 1, n+1
     do i = 1, m
      s = s + x(i) ** (k + j)   !c(k,j)の計算
     enddo
     c(k,j) = s
    enddo
    do i = 1, m
     t = t + y(i) * x(i) ** (k)  !b(k)の計算
    enddo
    b(k) = t
   enddo
  end subroutine squares
end module subprogs

  program main
   use subprogs
   implicit none
   integer m, n, k, i, j
   real(8), allocatable :: x(:), y(:), a(:), b(:), c(:,:)
   m = 10
   n = 3
   allocate (x(m), y(m), a(n+1), b(n+1), c(n+1,n+1))
   open(1,file = 'output6_09.d',status='old')    !データの読み込み
   do i = 1, m
    read(1,*) x(i), y(i)
    write(*,*) x(i), y(i)
   enddo
   close(1)
   call squares(x,y,b,c,m,n)
   call solve(c,a,b,n)
   write(*,*) 'coefficient of original function'
   write(*,*) '0.1'
   write(*,*) '0.2'
   write(*,*) '0.5'
   write(*,*) '1.0'
   write(*,*) 'coefficient of obtained function '
   do i = 1, n+1
   write(*,*) a(i)
   enddo
  end program main
   