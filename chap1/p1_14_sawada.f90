program p1_14
 implicit none
 real(8) x,y,er,ans
 integer i
 ans=1d0
 x=1
 y=1
 do i=1,10
   y=y*x/i
   ans=ans+y
   er=abs(ans-exp(1.0d0))
   write(*,*)'n=',i, 'e=',ans, 'er=',er
 end do
end program p1_14
 
 
 