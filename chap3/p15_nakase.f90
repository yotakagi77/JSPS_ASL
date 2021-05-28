!3,5,1,1で行列式が-2、逆行列が-0.5,2.5, 0.5, -1.5
module subprog
 implicit none
contains
!行列式と逆行列を求めるサブルーチン
 subroutine siki(a, b, d)
  real(8), intent(in) :: a(1:2, 1:2)
  real(8), intent(out) ::  b,  d(1:2, 1:2)
  integer i
!行列式
  b = a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1)
  if (b == 0) stop ' gyouretusiki = 0'
!逆行列
  d(1, 1) = a(2, 2) / b
  d(1, 2) = -1 * a(1, 2) / b
  d(2, 1) = -1 * a(2, 1) / b
  d(2, 2) = a(1, 1) / b
  write(*,*) 'gyouretusiiki ha', b
  write(*,*) ' gyakugyouretu ha'
  do i = 1, 2
   write(*,*) d(i, 1:2)
  end do
 end subroutine siki
end module subprog

program ensyu15
 use subprog
 implicit none
 real(8), allocatable :: x( :, :), x_1( :, :)
 real(8) xsiki
!2×2行列をランダムに設定
 allocate(x(2 , 2), x_1(2, 2))
 call random_number(x)
 write(*,*) 'x(: , :)= '
 read(*,*)x(: , :)
 call siki(x, xsiki, x_1)
end program ensyu15