program p1_11
 implicit none
 integer i,wa0,wa1
 wa0=0
 wa1=0
 do i=1,50
  wa0=wa0+(2*i-1)
  wa1=wa1+2*i
 end do
 write(*,*)'wa0, wa1, wa =',wa0, wa1, wa0+wa1
end program p1_11