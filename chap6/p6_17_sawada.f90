module subprogs
 implicit none
contains
 subroutine set_dbc(phi,x,n1,n2)
  integer, intent(in) :: n1,n2
  real(8), intent(in) :: x
  real(8), intent(out) :: phi(n1,n2)
  integer i,j
  real(8) pi
  pi = 2.0 * acos(0.0d0)
  phi(:,:) = 0.0d0
  do j = 1,n2
   do i = 1,n1
    if (j == 1) then
     phi(i,j) = sin(pi * x * (i-1))
    else if (j == n2) then
     phi(i,j) = 0.0d0
    else if (i == 1) then
     phi(i,j) = 0.0d0
    else if (i == n1) then
     phi(i,j) = 0.0d0
    end if
   end do
  end do
 end subroutine set_dbc

 subroutine chk_err(phi,c,d,n1,n2,er)
  real(8), intent(in) :: c,d,phi(n1,n2)
  integer, intent(in) :: n1,n2
  real(8) er,rhs
  integer i,j
  er = 0.0d0
  rhs = 0.0d0
  do j = 2,n2-1
   do i = 2,n1-1
    rhs = - c * (phi(i-1,j) + phi(i+1,j)) - d * (phi(i,j-1) + phi(i,j+1))
    er = er + (rhs - phi(i,j))**2
   end do
  end do
 end subroutine chk_err

 subroutine sor(phi,omg,x,n1,n2,c,d,er,er0,itrmax)
  integer, intent(in) :: n1,n2,itrmax
  real(8), intent(in) :: c,d,x,er0,omg
  real(8), intent(inout) :: phi(n1,n2)
  real(8) er,rhs
  integer i,j,itr
  call set_dbc(phi,x,n1,n2)
  do itr = 1,itrmax
   do j = 2,n2-1
    do i = 2,n1-1
      rhs = - c * (phi(i-1,j) + phi(i+1,j)) - d * (phi(i, j-1) + phi(i, j+1))
      phi(i,j) = phi(i,j) + omg * (rhs - phi(i,j))
    end do
  end do
  call chk_err(phi,c,d,n1,n2,er)
  write(*,*) ' itr,er = ',itr,er
  if ( er < er0) then
   write(*,*) '# conerged #'
   exit
  end if
 end do
 end subroutine sor

end module subprogs

program p6_17
 use subprogs
 implicit none
 integer, parameter :: n1 = 5, n2 = 5
 integer itr,i,j
 integer :: itrmax = 100
 real(8) phi(n1,n2),c,d,x,omg,rhs,er,pi,dx
 real(8) :: er0 = 1.0d-6
 pi = 2.0 * acos(0.0d0)
 dx = 1.0d0 / (n1-1)
 c = - dx**2 / (2 * (dx**2 + dx**2))
 d = c * (dx / dx)**2
 omg = 2 / (1 + sin(pi / (n1-1)))
 !omg = 1.5d0
 call sor(phi,omg,x,n1,n2,c,d,er,er0,itrmax)
 write(*,*) 'phi='
 do i = 1,n1
  write(*,*) (phi(i,j), j = 1,n2)
 end do
end program p6_17