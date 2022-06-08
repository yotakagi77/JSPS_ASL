module subprogs
 implicit none
contains
 subroutine allocate_vec(a)
  integer i, j, n 
  real(8), allocatable, intent(out) :: a(:,:)
  real(8), allocatable :: b(:,:), c(:,:), d(:,:)
  write(*,*) 'input n'
  read(*,*) n
  allocate (a(n,n))
  allocate (b(n,n))
  allocate (c(n,n))
  allocate (d(n,n))
  do i = 1, n
   do j = 1, n
    read(*,*) a(i,j)
   enddo
  enddo
  do i = 1, n
   do j = 1, n
    b(j,i) = a(i,j)
   enddo
  enddo
  do i = 1, n
   do j = 1, n
    c(i,j) = 0.5 * (a(i,j) + b(i,j))
    d(i,j) = 0.5 * (a(i,j) - b(i,j))
   enddo
  enddo 
  do i = 1, n
   write(*,*) (c(i,j), j = 1,n)
  enddo
  do i = 1, n
   write(*,*) (d(i,j), j = 1,n)
  enddo
 end subroutine allocate_vec
end module subprogs

program main
 use subprogs
 implicit none
 integer i,n,j
 real(8), allocatable :: a(:,:)
 call allocate_vec(a)
end program main
   
 
      