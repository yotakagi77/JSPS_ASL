program dot
implicit none
integer i
real  u(3),v(3),l,dotp,a,b,c

write(*,*) 'please input  u1'
read(*,*) u(1)
write(*,*) 'please input  u2'
read(*,*) u(2)
write(*,*) 'please input  u3'
read(*,*) u(3)

write(*,*) 'please input  v1'
read(*,*) v(1)
write(*,*) 'please input  v2'
read(*,*) v(2)
write(*,*) 'please input  v3'
read(*,*) v(3)


write(*,*) (u(i),i=1,3)
write(*,*) (v(i),i=1,3)

a = u(2) * v(3) - u(3) * v(2)

b = u(3) * v(1) - u(1) * v(3)

c = u(1) * v(2) - u(2) * v(1)

write(*,*) a, b, c

end program dot