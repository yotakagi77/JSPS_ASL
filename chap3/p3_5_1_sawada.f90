!ƒ‚ƒWƒ…[ƒ‹
module subprog
 implicit none
contains
 subroutine swap(a,b)
  integer, intent(inout)::a,b
  integer tmp
  tmp=a
  a=b
  b=tmp
 end subroutine swap
end module subprog

!ƒƒCƒ“
program exchange
 use subprog
 implicit none
 integer::x=77,y=9095,tmp=0
 write(*,*)'x=',x,'y=',y,'tmp=',tmp
 call swap(x,y)
 write(*,*)'x=',x,'y=',y,'tmp=',tmp
end program exchange