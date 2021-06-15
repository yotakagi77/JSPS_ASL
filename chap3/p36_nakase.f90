!https://www.geisya.or.jp/~mwm48961/linear_algebra/eigenvalue2.htmで行列の答え参照。
module subprog
 implicit none
contains
 function eval2_2mat(a) result(eval)
  real(8), intent(in) :: a(: , :)
  complex(8) eval(2)
  real(8) b, c, d, e
  if (size(a,1) /= size(a, 2)) stop 'not square'
  if (size(a, 1) /= 2) stop 'not 2_2 matrix'
  b  = - 0.5d0 * (a(1, 1) + a(2, 2))
  c = a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1)
  d = b ** 2 -c
  if (d < 0.0d0) then
   eval(1) = cmplx(-b, sqrt(-d), kind = 8)
   eval(2) = conjg(eval(1))
  else if (d > 0.0d0) then
   e = -b + sign(sqrt(d), -b)
   eval(1) = cmplx(e, 0.0d0, kind = 8)
   eval(2) = cmplx(c/e, 0.0d0, kind = 8)
  else
   eval(1) = cmplx(-b, 0.0d0, kind = 8)
   eval(2) = eval(1)
  end if
 end function eval2_2mat

!次のサブルーチンを加えた。
 subroutine bectoru(a, x, y, bec, bec_2, bec2, bec2_2)
  complex(8), intent(in) :: x, y
  real(8), intent(in) :: a(2,2)
!実数か複素数化で代入する値の宣言変える
  complex(8), intent(out) :: bec(1, 2), bec_2(1, 2)
  real(8), intent(out) :: bec2(1, 2), bec2_2(1, 2)
!ここで場合分け
  real(8) b, c, d, e
  integer i
  b  = - 0.5d0 * (a(1, 1) + a(2, 2))
  c = a(1, 1) * a(2, 2) - a(1, 2) * a(2, 1)
  d = b ** 2 -c
!d<0のとき複素数でベクトルを出す。ベクトルの出し方間違ってる可能性アリ、、、分母が0になる可能性があるから
  if (d < 0.0d0) then
!ベクトルの出し方が2通りあるので分母が0にならない方を使う
   if ( a(1,2) == 0.0d0 ) then
    bec(1, 1) = 1.0d0
    bec(1, 2) = - a(2,1) / (a(2, 2) - x)
    bec_2(1, 1) = 1.0d0
    bec_2(1, 2) = - a(2,1) / (a(2, 2) - y)
   else
    bec(1, 1) = 1.0d0
    bec(1, 2) = - (a(1, 1) - x) / a(1, 2)
    bec_2(1, 1) = 1.0d0
    bec_2(1, 2) = - (a(1, 1) - y) / a(1, 2)
   end if
   write(*, *) 'bekutoru1', bec(1, 1), bec(1, 2)
   write(*, *) 'bekutoru2', bec_2(1, 1), bec_2(1, 2)
!重解、実数解の場合はそのまま出す
 else
   if ( a(1,2) == 0.0d0 ) then
    bec2(1, 1) = 1.0d0
    bec2(1, 2) = - a(2,1) / (a(2, 2) - x)
    bec2_2(1, 1) = 1.0d0
    bec2_2(1, 2) = - a(2,1) / (a(2, 2) - y)
   else
    bec2(1, 1) = 1.0d0
    bec2(1, 2) = - (a(1, 1) - x) / a(1, 2)
    bec2_2(1, 1) = 1.0d0
    bec2_2(1, 2) = - (a(1, 1) - y) / a(1, 2)
   end if
   write(*, *) 'bekutoru1', bec2(1, 1), bec2(1, 2)
   write(*, *) 'bekutoru2', bec2_2(1, 1), bec2_2(1, 2)
  end if
 end subroutine bectoru

end module

program ensyu36
 use subprog
 implicit none
 real(8) a(2, 2), bec2(1, 2), bec2_2(1, 2)
 complex(8) ev(2), bec(1, 2), bec_2(1, 2)
 write(*, *) 'input a'
 read(*, *) a
 ev(:) = eval2_2mat(a)
 write(*, *) 'koyuuti', ev(1), ev(2)
 call bectoru(a, ev(1), ev(2), bec, bec_2, bec2, bec2_2)
end program ensyu36