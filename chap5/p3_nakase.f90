module mat_subprogs
 implicit none
contains
recursive function det_mat(a, n) result(det)
 integer, intent(in) :: n
 real(8), intent(in) :: a(n, n)
 real(8) det, b(n-1, n-1), t
 integer i
 if (n >1) then
  det = 0.0d0
   do i = 1, n
    b(1:i-1, 1:n-1) = a(1:i-1, 2:n)
    b(i:n-1, 1:n-1)= a(i+1:n, 2:n)
    det = det + (-1.0d0) ** (i + 1) * a(i,1) * det_mat(b, n-1)
   end do
  else
   det = a(1, 1)
  end if
 end function det_mat
end module mat_subprogs

program cal_det
 use mat_subprogs
 implicit none
 integer, parameter :: n=3
 integer i
 real(8) a(n, n), at(n,n), a_sankaku(n, n)
 real(8) t
 write(*,*)'input a'
 read(*,*) a
 at = transpose(a)
 a_sankaku( : , : ) = a( : , : )
 do i = 1, n
  a_sankaku(i+1 : n, i) = 0.0d0
 end do
 do i = 1, n
  write(*,*) a_sankaku(i, 1:n)
 end do
 write(*,*) 'det = ', det_mat(a, n)
 write(*,*) 'det_t= ', det_mat(at, n)
!対角要素の積
  t = 1.0d0
  do i = 1, n
   t = t * a_sankaku(i, i)
  end do
 write(*,*) t
 write(*,*) 'det_sankaku= ', det_mat(a_sankaku, n)
end program cal_det