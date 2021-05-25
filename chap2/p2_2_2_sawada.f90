program p2_2_1
 implicit none
 integer::ia(1:4)=(/1,2,3,4/)
 ia(2:4)=ia(1:3)
 write(*,*)'ia(2:4)=',ia(2:4)
end program p2_2_1