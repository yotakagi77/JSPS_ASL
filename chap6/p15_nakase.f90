module subprogs
 implicit none
contains
 subroutine gauss_seidel(a, b, x, n, itrmax, er0)
 !a:係数行列、b:右辺ベクトル、itrmax：最大反復数、er0: 誤差の閾値
 integer, intent(in) :: n, itrmax
 real(8), intent(in) :: a(n, n), b(n), er0
 real(8), intent(out) :: x(n)
 real(8) s, er, rd(n), r(n)
 integer i, itr
 call random_number(x(1:n))
 do i = 1, n
  if (a(i, i) == 0.0d0) stop 'a(i, i) == 0.0d0'
  rd(i) = 1.0d0 / a(i, i)
 end do
 x(1:n) = 0.0d0
 do itr = 1, itrmax
  do i = 1, n
   s = dot_product(a(i, 1:i-1), x(1:i-1))
   s = s + dot_product(a(i, i+1:n), x(i+1:n))
   x(i) = rd(i) * (b(i) - s)
  end do
  r(1:n) = b(1:n) - matmul(a, x)
  er = dot_product(r, r)
  write(*,*) 'itr = ', itr, ' err= ', er
  if ( er <= er0) then
   write(*,*) '# converged #'
   exit
  end if
 end do
 do i = 1, 4
  write(*,*) x(i)
 end do
 write(*,*) ' '
 end subroutine gauss_seidel
end module subprogs


program ensyu15
 use subprogs
 implicit none
 real(8) a(16, 16), b(16), phy(16), c, d, dx, t(4, 4), k(4), x1(4), pi
 real(8) phy_x(4), bd(12), a11(4, 4), a12(4, 12), ab(4), phy_e(2), phy_o(2), be(2), bo(2), a_x(4,4)
!phyはφ、aは係数行列A, bは右辺のベクトル, ｔ・k・x1は境界以外の連立方程式で使う。
 real(8) :: er0 = 1.0d-6
 integer ::  i, itrmax=100, n = 4
 pi = 2.0d0 * acos(0.0d0)
 dx =  0.33333333d0
 c = - dx**2 / (2 * (dx**2 + dx**2))
 d = c * (dx / dx) ** 2
!いったん係数行列を全部0にして値を変えていく
 a(1:16, 1:16) = 0.0d0
 do i = 1, 16
  a(i, i) = 1.0d0
!4以下のときは値が決まる境界上の点
   if ( i <4 ) then
    phy(i) = sin(pi * (i - 1.0d0) * dx)
    b(i) = phy(i)
!6,7,10,11のときは境界以外の点。5点差分法の式より非ゼロ成分が決まる。
   else if ( i == 6 .or. i == 7 .or. i == 10 .or. i == 11) then
    a(i, i-4) = d
    a(i, i-1) = c
    a(i, i+1) = c
    a(i, i+4) = d
    b(i) = 0.0d0
   else
!それ以外の境界値は全部0
    phy(i) = 0.0d0
    b(i) = phy(i)
   end if
 end do
!5点差分法より境界値以外の点を求める。
!tは係数行列Aの6~7、10~11を取り出したもの
  t(1, 1:2) = a(6, 6:7)
  t(1, 3:4) = a(6, 10:11)
  t(2, 1:2) = a(7, 6:7)
  t(2, 3:4) = a(7, 10:11)
  t(3, 1:2) = a(10, 6:7)
  t(3, 3:4) = a(10, 10:11)
  t(4, 1:2) = a(11, 6:7)
  t(4, 3:4) = a(11, 10:11)
  k(1) = -c * (phy(2) + phy(5))
  k(2) = -c * (phy(3) + phy(8))
  k(3) = -c * (phy(9) + phy(14))
  k(4) = -c * (phy(12) + phy(15))
  x1(1) = phy(6)
  x1(2) = phy(7)
  x1(3) = phy(10)
  x1(4) = phy(11)
  call gauss_seidel(t, k, x1, n, itrmax, er0)
  phy(6) = x1(1)
  phy(7) = x1(2)
  phy(10) = x1(3)
  phy(11) = x1(4)
  !A11とA22、φと右辺を定める
  a11(1:2, 1:2) = a(6:7, 6:7)
  a11(3:4, 1:2) = a(10:11, 6:7)
  a11(1:2, 3:4) = a(6:7, 10:11)
  a11(3:4, 3:4) = a(10:11, 10:11)
  a12(1:2, 1:5) = a(6:7, 1:5)
  a12(1:2, 6:7) = a(6:7, 8:9)
  a12(1:2, 8:12) = a(6:7, 12:16)
  a12(3:4, 1:5) = a(10:11, 1:5)
  a12(3:4, 6:7) = a(10:11, 8:9)
  a12(3:4, 8:12) = a(10:11, 12:16)
 phy_x(1:2) = phy(6:7)
 phy_x(3:4) = phy(10:11)
 bd(1:5) = b(1:5)
 bd(6:7) = b(8:9)
 bd(8:12) = b(12:16)
 write(*,*) ' '
 ab(:) = - matmul(a12, bd)
 !最後に式（6.56)を作る
 a_x(1 ,1) = a11(1, 1)
 a_x(1 ,2) = a11(4, 1)
 a_x(1 ,3) = a11(2, 1)
 a_x(1 ,4) = a11(3, 1)
 a_x(2 ,1) = a11(1, 4)
 a_x(2 ,2) = a11(4, 4)
 a_x(2 ,3) = a11(2, 4)
 a_x(2 ,4) = a11(3, 4)
 a_x(3 ,1) = a11(1, 2)
 a_x(3 ,2) = a11(4, 2)
 a_x(3 ,3) = a11(2, 2)
 a_x(3 ,4) = a11(3, 2)
 a_x(4 ,1) = a11(1, 3)
 a_x(4 ,2) = a11(4, 3)
 a_x(4 ,3) = a11(2, 3)
 a_x(4 ,4) = a11(3, 3)
phy_e(1) = phy_x(1)
phy_e(2) = phy_x(4)
phy_o(1) = phy_x(2)
phy_o(2) = phy(3)
be(1) = ab(1)
be(2) = ab(4)
bo(1) = ab(2)
bo(2) = ab(3)
write(*,*) 'a_x'
do i = 1, 4
  write(*,*) a_x(i, 1:4)
end do
write(*,*) 'phy_e, phy_o, bd, be '
do i = 1,2
  write(*,*) phy_e(i), phy_o(i), bd(i), be(i)
end do
end program ensyu15
