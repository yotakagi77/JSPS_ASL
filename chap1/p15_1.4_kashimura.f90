program p15_kashimura
 implicit none

 integer :: i
 
  write(*,*)""
  write(*,*)"do i=1,4"
   do i=1,4
    write(*,*)i
   end do
   
  write(*,*)""
  write(*,*)"do i=1,4,2"
   do i=1,4,2
    write(*,*)i
   end do

  write(*,*)""
  write(*,*)"do i=4,1,-2"
   do i=4,1,-2
    write(*,*)i
   end do

!始値、終値増分値が良くないパターン   
  write(*,*)""
  write(*,*)"do i=1,4,-2"
   do i=1,4,-2
    write(*,*)i
   end do

  write(*,*)""
  write(*,*)"do i=0,0,-1"
   do i=0,0,-1
    write(*,*)i
   end do

 stop
end program p15_kashimura