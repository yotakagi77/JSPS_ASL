program p2_2_1
 implicit none
 integer::ia(1:4)=(/1,2,3,4/),i
 do i=2,4
  ia(i)=ia(i-1)
 end do
 write(*,*)'i(a)=',ia(i)
end program p2_2_1