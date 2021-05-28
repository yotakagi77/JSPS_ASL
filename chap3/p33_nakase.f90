! 1,-2,-3,2,1,-1,1,3,2が-20になればよい
module subprog
 implicit none
contains
 subroutine gaiseki(x, y, z)
 real(8), intent(in) :: x(3), y(3), z(3)
 real(8) xg(2 ,2), yg(2, 2), zg(2, 2), c(2, 3), m1, m2, m3, t(3), q
 integer i
!ここで2つのベクトルをつなげる
 c(1, 1:3) = y(1:3)
 c(2, 1:3) = z(1 :3)
 do i = 1, 2
  write( * , *) 'c(:)= ', c( i, 1:3)
 end do
!3つの行列を抽出
 xg(:, :) = c(1:2, 2:3)
 yg(:, :) = c(1:2 , 3:1:-2)
 zg(:, :) = c(1:2, 1:2)
 do i = 1, 2
  write( * , *) 'xg(:)= ', xg( i, 1:2)
end do
 do i = 1, 2
  write( * , *) 'yg(:)= ', yg( i, 1:2)
 end do
 do i = 1, 2
  write( * , *) 'zg(:)= ', zg( i, 1:2)
 end do
!外積の各成分を計算
 m1 = xg(1, 1) * xg(2, 2) - xg(1, 2) * xg(2, 1)
 m2 = yg(1, 1) * yg(2, 2) - yg(1, 2) * yg(2, 1)
 m3 = zg(1, 1) * zg(2, 2) - zg(1, 2) * zg(2, 1)
 t(1) = m1
 t(2) = m2
 t(3) = m3
 do i = 1, 3
  write(*,*) 't', t(i)
 end do
 write(*,*) 'kotae ha ', dot_product(x, t)
 end subroutine gaiseki
end module subprog


program ensyu33
use subprog
implicit none
real(8) a( 3 ), b( 3 ),c(3)
write(*,*) 'a and b and c'
read(*,*) a(:), b(:), c(:)

call gaiseki(a, b, c)
end program ensyu33