module subprog
 implicit none
contains
 function normal_vec2(v) result(nv)
 real(8), intent(in) :: v(:)
 real(8) nv(size(v, 1)), vl
 vl = sqrt(dot_product(v, v))
 if (vl == 0.0d0) then
  nv(:) =0.0d0
 else
  nv(:) = v(:) / vl
end if
 end function normal_vec2


 function gs(a) result(e)
 real(8), intent(in) :: a(3, 3)
 real(8) e(3, 3), dotp
 integer k, j
 e(1:3, 1) = normal_vec2(a(1:3, 1))
 do k = 2, 3
  e(1:3, k) = a(1:3, k)
  do j = 1, k-1
   dotp = dot_product(a(1:3, k), e(1:3, j))
   e(1:3, k) = e(1:3, k) - dotp * e(1:3, j)
  end do
  e(1:3, k) = normal_vec2(e(1:3, k))
 end do
 end function gs

!行列Cを求める
 function tat(t, a) result(c)
 real(8), intent(in) :: t(3, 3), a(3, 3)
 real(8) x(3, 3), y(3, 3), c(3, 3)
 integer i, j, k
 y = transpose(t)
 do j=1,3
    do i=1,3
        x(i, j) = 0.0d0
        do k=1,3
         x(i,j) = x(i,j)+y(i,k)*a(k,j)
        end do
    end do
end do
 do j=1,3
    do i=1,3
        c(i, j) = 0.0d0
        do k=1,3
         c(i,j) = c(i,j)+x(i,k)*t(k,j)
        end do
    end do
end do
 write(*,*)' '
 end function tat

!行列式をそれぞれ求めるサブルーチン
 subroutine siki(a)
 real(8), intent(in) :: a(3, 3)
 real(8) siki_a
 real(8) x, y, z
 x = a(2,2) * a(3,3) - a(2,3) * a(3,2)
 y = a(2,1) * a(3,3) - a(2,3) * a(3,1)
 z = a(2,1) * a(3,2) - a(2,2) * a(3,1)
 siki_a = a(1, 1) * x - a(1,2) * y + a(1, 3) * z
 write(*,*) siki_a
 end subroutine 

end module


program ensyu39
use subprog
implicit none
real(8) a(3, 3), b(3,3), e(3,3), c(3, 3)
integer i
call random_number(a(1:3, 1:3))
do i = 1, 3
 write(*,*) a(i, 1:3)
end do
write(*,*)' '
call random_number(b(1:3, 1:3))
do i = 1, 3
 write(*,*) b(i, 1:3)
end do
e(:, :) = gs(b)
c(:, :) = tat(e, a)
call siki(a)
call siki(c)
end program ensyu39