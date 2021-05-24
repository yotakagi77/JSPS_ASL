program p1_2
 implicit none
 integer i,wa1,wa2
 wa1=0
 do i=1,10
  wa1=wa1+i
 end do
 wa2=(10+1)*10*1/2
 write(*,*)'wa1=',wa1,'wa2=',wa2
end program p1_2