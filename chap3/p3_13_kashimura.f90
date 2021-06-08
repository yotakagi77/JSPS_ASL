!p97 演習3.13
module vec_subprogs      
  implicit none
contains

  subroutine allocate_rvec(r,n)
    integer,intent(out) :: n
    double precision,allocatable,intent(out) :: r(:)
	write(*,fmt='(a)',advance='no') "input n : "
    read(*,*) n
    !乱数で埋める
    allocate (r(n))
    call random_seed
    call random_number(r(1:n))
    r(1:n)=2.0d0*r(1:n)-1.0d0
!    write(*,*) "a(n)=" r(1:n)
  end subroutine allocate_rvec
  
  subroutine print_vec(a,n)
    double precision,intent(in) :: a(:)
	integer,intent(in) :: n
	write(*,*) "matrix a(n)"
	write(*,'(100e12.4)') a(1:n)
  end subroutine print_vec
  
end module vec_subprogs

program random_vec
  use vec_subprogs
  implicit none
    double precision,allocatable :: v(:)
	integer :: n
    call allocate_rvec(v,n)
	call print_vec(v,n)
end program random_vec