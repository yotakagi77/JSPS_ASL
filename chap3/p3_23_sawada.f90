!モジュール
module  subprogs
 implicit none
contains
 function func_det(a,i) result(det)
 integer, intent(in)::i
 integer j
 real(8), intent(in)::a(1:3,1:3)
 real(8) d,e,b(1:2,1:3),c(1:2,1:2),det
 
 b(1:i-1,1:3)=a(1:i-1,1:3)     !i行目を抜く
 b(i:2,1:3)=a(i+1:3,1:3)

 det=0.0d0
 do j=1,3                      !j列目を抜いて行列式出すまでをループ
  c(1:2,1:j-1)=b(1:2,1:j-1)
  c(1:2,j:2)=b(1:2,j+1:3)
  d=c(1,1)*c(2,2)-c(1,2)*c(2,1) !小行列の行列式
  e=(-1)**(i+j)*d               !余因子
  det=det+a(i,j)*e              !行列式の計算
 end do

 end function func_det
end module subprogs

!メイン
program p3_23
 use subprogs
 implicit none
 real(8) a(1:3,1:3)
 integer i
 write(*,*) 'Input a'
 read(*,*) a(1:3,1:3)
 write(*,*) 'Input i'
 read(*,*) i
 write(*,*) '|A|=',func_det(a,i)
end program p3_23
 