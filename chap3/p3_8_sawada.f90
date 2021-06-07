!モジュール
module subprog
 implicit none
contains
 function func_touhi_wa(a,r,n) result(s)
  real(8), intent(in)::a,r
  integer, intent(in)::n
  real(8) s
  s=a*(1-r**n)/(1-r)         !和を計算
 end function func_touhi_wa
end module subprog

!メイン
program p3_8
 use subprog
 implicit none
 real(8) a,r
 integer n
 write(*,*) 'Input a,r,n'
 read(*,*) a,r,n
 write(*,*) 's=',func_touhi_wa(a,r,n)
end program p3_8
