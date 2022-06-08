module subprog
 implicit none
contains
 function adjugate(i,j,a) result(adj)
    integer i, j, k, l
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
end module subprog

program main
 use subprog
 implicit none 
 integer :: i, j
 real(8) :: a(3,3)
 call random_number(a(1:3, 1:3))
 do i = 1,3
  write(*,*) (a(i,j),j=1,3)
 enddo
 write(*,*) 'input i (1 or 2 or 3)'
 read(*,*) i 
 if (i <= 0 .or. i > 3) stop 'stop, i is wrong'
 write(*,*) 'input j (1 or 2 or 3)'
 read(*,*) j
 if (j <= 0 .or. j > 3) stop 'stop, j is wrong'

 write(*,*) 'adjugate = ', adjugate(i,j,a)

end program main
