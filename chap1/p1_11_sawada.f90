program p1_11
 implicit none
 integer a,p,ans
 write(*,*) 'Input a and p'
 read(*,*) a,p
 ans=a-int(a/p)*p
 write(*,*) 'mod(a,p)=',ans
end program p1_11