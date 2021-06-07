!モジュールサブルーチン
module subprog
 implicit none
contains
 subroutine swap(a,b,c)
  integer a,b,c
  c=a+b
 end subroutine swap
end module subprog

!主プログラム
program p3_2
 use subprog
 implicit none
 integer :: i=45,j=55
 integer k
 call swap(i,j,k)
 write(*,*) 'i=',i,'j=',j,'k=',k
end program p3_2