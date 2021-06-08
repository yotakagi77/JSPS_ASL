module subprog
implicit none
contains
 function det(a) result(t)
 real(8), intent(in) :: a(1:3, 1:3)
 real(8)  x, y, z, t
!行列式
 x = a(2,2) * a(3,3) - a(2,3) * a(3,2)
 y = a(2,1) * a(3,3) - a(2,3) * a(3,1)
 z = a(2,1) * a(3,2) - a(2,2) * a(3,1)
!対角要素
 t = a(1,1) * x - a(1,2) * y + a(1,3) * z
 end function det
end module

program ensyu24
use subprog
implicit none
real(8) b(3,3), bt(3,3), btri(3,3), detb, detbt, detbtri
integer i
call random_number(b(1:3, 1:3))
do i = 1, 3
 write(* , *) 'b = ', b(i, 1:3)
end do
detb = det(b)
write(*,*) detb
!転置させる
bt(: , :) = transpose(b)
do i = 1, 3
 write(* , *) 'bt = ', bt(i, 1:3)
end do
detbt = det(bt)
write(*,*) detbt

!上三角行列を求める
btri( : , : ) = b( : , : )
do i = 1, 3
 btri(i+1 : 3, i) = 0.0d0
end do
detbtri = det(btri)
write(*,*) detbtri
write(*,*) 'detbtri', btri(1,1) * btri(2,2) * btri(3,3)
end program ensyu24