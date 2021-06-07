!モジュール
module subprogs
 implicit none
contains
 function func_det(d) result(m)
 real(8), intent(in)::d(:,:)
 real(8) m

 m=(d(1,1)*d(2,2))-(d(1,2)*d(2,1))　!行列式を計算

 end function func_det
end module subprogs


!メイン
program p3_20
 use subprogs
 implicit none
 real(8) a(2,2),b(2,2),c(2,2)

 write(*,*) 'Input a'           !A,Bを決める
 read(*,*) a(:,:)
 write(*,*) 'Input b'
 read(*,*) b(:,:)

 write(*,*) '|A|=',func_det(a)              !行列式を出力
 write(*,*) '|B|=',func_det(b)
 write(*,*) '|A||B|=',func_det(a)*func_det(b)
 c(:,:)=matmul(a,b)                          !ABをcとする
 write(*,*) '|AB|=',func_det(c)

end program p3_20
 