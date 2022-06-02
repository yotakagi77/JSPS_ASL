module subprogs
 implicit none
contains
 subroutine solve(a0,x,b,n)
  ! --- ガウス消去法(部分pivotなし)---
  integer, intent(in) :: n
  real(8), intent(in) :: a0(n,n), b(n)
  real(8), intent(out) :: x(n)
  integer i, j, k
  real(8) ar, a(n,n), d
  a(:,:) = a0(:,:)
  x(:) = b(:)
  !前進消去
  do k = 1, n
   if(a(k,k) == 0.0d0) stop 'pivot = 0'
   ar = 1.0d0 / a(k,k)
   a(k,k) = 1.0d0
   a(k,k+1:n) = ar * a(k,k+1:n)
   x(k) = ar * x(k)
   do i = 1, n
    if(i/=k) then
      a(i,k+1:n) = a(i,k+1:n) - a(i,k) * a(k,k+1:n)
      x(i) = x(i) - a(i,k) * x(k)
      a(i,k) = 0.0d0
    endif
   enddo
  enddo
  !後退代入
  do i = 1, n-1
      d = x(i+1)
      do j = i + 1, n
        d = d - a(i, j) * d
      end do
  end do
  end subroutine solve

  subroutine set_random_ab(a,b,x,n)
   integer :: i, j, n
   real(8), allocatable :: a(:,:), b(:), x(:)
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
 write(*,*) 'x = '
 do i = 1, n
  write(*,*) x(i)
 enddo
end program main