program p2_8_1
 implicit none
 real(8) a(3),b(3),crossp(3),l,ans
 write(*,*) 'Input P(x,y,z)'
 read(*,*) a(1:3)
 write(*,*) 'Input Q(x,y,z)'
 read(*,*) b(1:3)
 crossp(1)=a(2)*b(3)-b(2)*a(3)
 crossp(2)=a(3)*b(1)-b(3)*a(1)
 crossp(3)=a(1)*b(2)-b(1)*a(2)
 write(*,*) crossp(1:3)
 l=sqrt(crossp(1)**2+crossp(2)**2+crossp(3)**2)
 ans=l/2
 write(*,*)'A=',ans
end program p2_8_1