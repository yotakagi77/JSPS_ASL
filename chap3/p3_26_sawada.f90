module subprogs
 implicit none
contains
 subroutine set_gridx(x,n1,n2)
  integer, intent(in)::n1,n2
  real(8), intent(out)::x(1:2,1:n1,1:n2)
  integer i,j
  do j=1,n2
   do i=1,n1
    x(1,i,j)=(dble(i)-1)/(dble(n1)-1)
    x(2,i,j)=(dble(j)-1)/(dble(n2)-1)
   end do
  end do
 end subroutine set_gridx

 function theory(x,n1,n2) result(phi)
  integer, intent(in)::n1,n2
  real(8), intent(in)::x(:,:,:)
  integer i,j
  real(8) pi,phi(1:n1,1:n2)
  pi=4.0d0*atan(1.0d0)
  do j=1,n2
   do i=1,n1
    phi(i,j)=sin(pi*x(1,i,j))*sinh(pi*(1-x(2,i,j)))/sinh(pi)
   end do
  end do
 end function theory
end module subprogs

program p3_26
 use subprogs
 implicit none
 integer i,j,n1,n2
 real(8),allocatable::x(:,:,:),phi(:,:)
 write(*,*) 'Input n1,n2'
 read(*,*) n1,n2
 allocate (x(1:2,1:n1,1:n2))
 allocate (phi(1:n1,1:n2))
 call set_gridx(x,n1,n2)
 phi(1:n1,1:n2)=theory(x,n1,n2)
 do i=1,n1
  write(*,*) (phi(i,j),j=1,n2)
 end do
end program p3_26
 