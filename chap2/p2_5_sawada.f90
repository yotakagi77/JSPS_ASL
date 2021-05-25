program p2_5
 implicit none
 real(8) u(3),a(3)
 integer l
 write(*,*) 'Input u(1), u(2) and u(3)'
 read(*,*) u(1:3)
 l=sqrt(u(1)**2+u(2)**2+u(3)**2)
 if (l == 0) then
  stop 'stop (l == 0)'
 endif
 a(1:3)=u(1:3)/l
 write(*,*)'a(1:3)=',a(1:3)
end program p2_5