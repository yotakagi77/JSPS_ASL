!モジュール
module subprogs
 implicit none
contains
 subroutine keisan(a,m)
 integer, intent(in)::m
 real(8), intent(in)::a(m)
 integer i
 real(8) sum,avg,var,sd
 sum=0
 do i=1,m
  sum=sum+a(i)             !合計
  avg=sum/m                !平均　　　
  var=((a(i)-avg)**2)/(m-1) !分散
  sd=sqrt(var)             !標準偏差
 end do
 write(*,*) 'sum,avg,var,sd=',sum,avg,var,sd
 end subroutine keisan
end module subprogs

!メイン
program p3_14
 use subprogs
 implicit none
 integer, parameter::n=4   !要素数
 real(8) x(n)
 call random_seed
 call random_number(x(1:n))
 write(*,*) 'x=',x(1:n)
 call keisan(x,n)
end program p3_14
 