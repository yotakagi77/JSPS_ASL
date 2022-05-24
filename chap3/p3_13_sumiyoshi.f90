module vec_subprogs
 implicit none
contains
 subroutine allocate_rvec(v,n)
  real(8), allocatable, intent(out) :: v(:)
  integer n
  write(*,'(a)',advance = 'no') 'input n : '
  read(*,*) n
  if(n < 1 ) then
  write(*,*) 'input integer n'
  stop 
  endif
  allocate(v(n))
  call random_number(v)
  v(1:n) = 2.0 * v(1:n) - 1.0
 end subroutine allocate_rvec

 subroutine print_vec(v,n)
  real(8), intent(in) :: v(:)
  integer i, n
  n = size(v,1)
   do i = 1, n
     write(*,'(100e12.4)',advance = 'no') v(i)
   enddo
 end subroutine print_vec
end module vec_subprogs

program random_vec
  use vec_subprogs
  implicit none
  real(8), allocatable :: v(:)
  integer n
  call allocate_rvec(v,n)
  call print_vec(v,n)
end program random_vec
