module mat_subprogs
 implicit none
contains
 recursive function det_mat(a) result(det)
  real(8),intent(in)::a(:,:)
  integer i,n
  real(8), allocatable::b(:,:)
  real(8) det
  n=size(a,1)
  allocate (b(1:n-1,1:n-1))
  if (n > 1) then
   det=0.0d0
   do i=1,n
    b(1:i-1,1:n-1)=a(1:i-1,2:n)
    b(i:n-1,1:n-1)=a(i+1:n,2:n)
    det=det+(-1.0d0)**(i+1)*a(i,1)*det_mat(b)
   end do
  else
   det=a(1,1)
  end if
 end function det_mat
end module mat_subprogs

program cal_det
 use mat_subprogs
 implicit none
 integer, parameter::n=3
 integer i,j
 real(8) a(1:n,1:n)
 write(*,*) 'Input A'
 read(*,*) a(1:n,1:n)
 write(*,*) 'A='
 do i=1,n
  write(*,*) (a(i,j),j=1,n)
 end do
 write(*,*) 'det=',det_mat(a)
end program cal_det