module subprogs
 implicit none
contains
 function adjugate(i,j,a) result(adj)
    integer ::  i, j, k
    real(8) det, c, adj
    real(8) :: a(3,3), b(2,2)
    do k = 1, 3
        b(1:i-1, 1:j-1) = a(1:i-1, 1:j-1)
        b(1:i-1, j:3-1) = a(1:i-1, j+1:3)
        b(i:2, 1:j-1) = a(i+1:3, 1:j-1)
        b(i:2, j:2) = a(i+1:3, j+1:3)
    end do
    det = b(1,1) * b(2,2) - b(1,2) * b(2,1)
    c = (-1.0d0) ** (i + j)
    adj = c * det
 end function adjugate

 function func_det(i, a, adj) result(det)
        integer :: i, j
        real(8)  :: a(3,3), adj(3,3)
        real(8) det
        det = 0.0d0
        do j = 1,3
                det = det + a(i,j) * adj(i,j)
        end do
 end function func_det

 function rev(c,det) result(a_rev)
   real(8) :: a_rev(3,3), c(3,3), det(3)
   integer :: i, j
   do i = 1, 3
    do j = 1,3
     a_rev(i,j) = c(i,j) / det(1)
    enddo
   enddo
 end function rev
end module subprogs

program main
 use subprogs
 implicit none 
 integer :: i, j
 real(8) :: a(3,3),b(3,3),c(3,3),adj(3,3),a_rev(3,3),det(3),seki(3,3)
 call random_number(a(1:3, 1:3))
 do i = 1,3
  do j = 1,3
   adj(i,j) = adjugate(i,j,a)
  enddo
 enddo
 c(:,:) = transpose(adj)
 do i = 1,3
  write(*,*) (adj(i,j), j = 1,3)
 enddo
 do i = 1,3
  write(*,*) (c(i,j), j = 1,3) 
 enddo
 do i = 1, 3
  det(i) = func_det(i, a, adj)
 end do
 if(det(1) == 0) then 
 write(*,*) '|A| = 0, so stop processing'
 stop
 endif
 write(*,*) '|A| = ', det(1)

 a_rev(:,:) = rev(c,det)

 do i = 1,3
  write(*,*) (a_rev(i,j), j = 1,3)
 enddo

 seki = matmul(a,a_rev)
 
 do i = 1,3
  write(*,*) (seki(i,j), j = 1,3)
 enddo
 
end program main
