module subprogs
 implicit none
contains
 subroutine sor(a,b,x,n,itrmax,er0,omega)
  integer, intent(in) :: n, itrmax
  real(8), intent(in) :: a(n,n), b(n), er0,omega
  real(8), intent(out) :: x(n)
  real(8) s, er, rd(n), r(n)
  integer i, itr
  do i = 1, n
   if(a(i, i) == 0.0d0) stop 'a(i, i) == 0.0d0'
   rd(i) = 1.0d0 / a(i, i)
  enddo
  x(1:n) = 0.0d0
  do itr = 1, itrmax
   do i = 1, n
    s = dot_product(a(i, 1:i-1), x(1:i-1))
    s = s + dot_product(a(i, i+1: n), x(i+1:n))
    x(i) = x(i) + omega * (rd(i) * (b(i) - s ) - x(i))
   enddo
   r(1 : n) = b(1 : n) - matmul(a, x)
   er = dot_product(r, r)
   write(*,*) 'itr =', itr, 'err = ', er
   if(er <= er0 ) then
    write(*,*) '#converged#'
    exit
   endif
  enddo
 end subroutine sor

 subroutine alloc_dd_mat(a,b,x,n)
  integer n, i, j
  real(8), allocatable ::  a(:,:), b(:), x(:)
  real(8) omega
  write(*,*) 'input n'
  read(*,*) n 
  allocate (a(n,n), b(n), x(n))
    do i=1,n
        do j=1,n
            if(i==j) then
                a(i,j)=1.0d0
            else
                call random_seed
                call random_number(a(i,j))
            end if
        end do
    end do
  call random_number(b)
 end subroutine alloc_dd_mat
end module subprogs

program main
 use subprogs
 implicit none
 real(8), allocatable :: a(:,:), b(:), x(:)
 integer :: n, itrmax = 100, i
 real(8) :: er0 = 1.0d-6, omega = 1.0
 call alloc_dd_mat(a,b,x,n)
 call sor(a,b,x,n,itrmax,er0,omega)
 write(*,*) 'x(i)='
 do i = 1, n
  write(*,*) x(i)
 enddo
end program main


