module subprogs
 implicit none
contains
 subroutine solve(a0,x,b,n)
  !ガウスジョルダン法(部分pivot選択あり)
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n,n), b(n)
  real(8), intent(out) :: x(n)
  integer i, k, m, count, j
  real(8) ar, am, t, a(n,n), w(n), D, c
  a(:,:) = a0(:,:)
  x(:) = b(:)
  !部分pivot選択
  count = 0
  c = 1
  D = 1
  do k = 1, n
   m = k
   am = abs(a(k,k))
   do i = k + 1, n
    if(abs(a(i,k)) > am) then
     am = abs(a(i,k))
     m = i
    endif
   enddo
   if(am == 0.0d0) stop 'A is singular'
   if(k /= m) then
    w(k:n) = a(k,k:n)
    a(k,k:n) = a(m,k:n)
    a(m,k:n) = w(k:n)
    t = x(k)
    x(k) = x(m)
    x(m) = t
    count = count + 1
   endif
   !以下は通常のガウスジョルダン法の演算
   ar = 1.0d0 / a(k,k)
   a(k,k) = 1.0d0
   a(k,k+1:n) = ar * a(k,k+1:n)
   x(k) = ar * x(k)
   do i = 1, n
    if(i/=k) then
      a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(k,k+1:n)
      x(i) = x(i) - a(i,k) * x(k)
      c = c * ar
      a(i,k) = 0.0d0

    endif
   enddo
  enddo
  write(*,*) 'count = ', count, 'c = ', c
  write(*,*) 'a ='
  do i = 1, n
   write(*,*) (a(i,j), j = 1,n)
  enddo
  write(*,*) 'x = '
  do i = 1, n
    write(*,*) x(i)
  enddo
  do i = 1, n
   D = D * a(i,i)
  enddo
  write(*,*) '|A| = ', D * c * ((-1.0) ** (count))
 end subroutine solve

 subroutine set_random_ab(a,b,x,n)
   integer :: i, j, n
   real(8), allocatable :: a(:,:), b(:), x(:)
   write(*,*) 'input n'
   read(*,*) n
   allocate (a(n,n), b(n), x(n))
   call random_number(a)
   call random_number(b)
   do i = 1,n
    write(*,*) (a(i,j), j = 1,n)
   enddo
  end subroutine set_random_ab
end module subprogs

program main
 use subprogs
 implicit none
 real(8), allocatable :: a(:,:), b(:), x(:)
 integer :: i, n
 call set_random_ab(a,b,x,n)
 call solve(a,x,b,n)

end program main