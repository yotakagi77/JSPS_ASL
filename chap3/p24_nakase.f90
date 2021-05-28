module subprog
implicit none
contains
 subroutine siki(a, t)
 real(8), intent(in) :: a(1:3, 1:3)
 real(8), intent(out) :: t
 real(8)  x, y, z
 x = a(2,2) * a(3,3) - a(2,3) * a(3,2)
 y = a(2,1) * a(3,3) - a(2,3) * a(3,1)
 z = a(2,1) * a(3,2) - a(2,2) * a(3,1)
 t = a(1,1) * x - a(1,2) * y + a(1,3) * z
 write(*,*) t
 end subroutine siki
end module

program ensyu24
use subprog
implicit none
real(8) b(3,3), bt(3,3), bsankaku(3,3), bsiki, btsiki, bsankaku_siki
integer i
call random_number(b(1:3, 1:3))
do i = 1, 3
 write(* , *) 'b = ', b(i, 1:3)
end do
call siki(b, bsiki)
!転置させる
bt(: , :) = transpose(b)
do i = 1, 3
 write(* , *) 'bt = ', bt(i, 1:3)
end do
call siki(bt, btsiki)

!上三角行列を求める
bsankaku( : , : ) = b( : , : )
do i = 1, 3
 bsankaku(i+1 : 3, i) = 0.0d0
end do
call siki(bsankaku, bsankaku_siki)
write(*,*) 'sankakugyouretu_siki', bsankaku(1,1) * bsankaku(2,2) * bsankaku(3,3)
end program ensyu24