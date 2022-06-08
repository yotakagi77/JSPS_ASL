module mat_subprogs
 implicit none
contains
 recursive function det_mat(a,n) result(det)
  integer, intent(in) :: n
  real(8), intent(in) :: a(n,n)
  real(8) det, b(n-1,n-1)
  integer i
  if(n>1) then 
   det = 0.0d0
   do i = 1,n
    b(1:i-1,1:n-1) = a(1:i-1,2:n)
    b(i:n-1,1:n-1) = a(i+1:n,2:n)
    det = det + (-1.0d0) ** (i + 1) * a(i,1) * det_mat(b,n-1)
   enddo

 else 
   det = a(1,1)
 endif
 end function det_mat
end module mat_subprogs

program cal_det
 use mat_subprogs
 implicit none
 integer i, j, n
 real(8), allocatable :: a(:,:), b(:,:), c(:,:)
 real(8) s
 write(*,*) 'input n'
 read(*,*) n
 allocate (a(n,n), b(n,n), c(n,n))
 read(*,*) a

 write(*,*) 'A =' 
 do i = 1,n
  write(*,*) (a(i,j), j = 1,n) 
 enddo
 write(*,*) '|A| = ', det_mat(a,n)

 b = transpose(a)
 write(*,*) 'A^T =' 
 do i = 1,n
  write(*,*) (b(i,j), j = 1,n) 
 enddo
 write(*,*) '|A|^T = ', det_mat(b,n)

 c = a

 do i=1,3
  do j=1,3
   if (i>j) then
    c(i,j)=0.0d0
   end if
  end do
 end do

 write(*,*) "A_t=" 
 do i = 1,n
  write(*,*) (c(i,j), j = 1,n) 
 enddo
 s = 1.0
 do i = 1,n
  s = s * c(i,i)
 enddo
 write(*,*) '|A_t| = ', det_mat(c,n)
 write(*,*) 'seki = ', s
end program cal_det