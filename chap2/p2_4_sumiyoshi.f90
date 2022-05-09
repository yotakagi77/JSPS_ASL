program dot
implicit none
integer i
real  u(3),v(3),l,dotp

write(*,*) 'please input  u1'
read(*,*) u(1)
write(*,*) 'please input  u2'
read(*,*) u(2)
write(*,*) 'please input  u3'
read(*,*) u(3)

write(*,*) (u(i),i=1,3)

l = (u(1)**2+u(2)**2+u(3)**2)**0.5

write(*,*) 'l=',l

write(*,*) 'please input  v1'
read(*,*) v(1)
write(*,*) 'please input  v2'
read(*,*) v(2)
write(*,*) 'please input  v3'
read(*,*) v(3)

dotp = 0

do i = 1,3
dotp = dotp + u(i) * v(i)
enddo

write(*,*) 'dot product=', dotp

write(*,*) 'dot product=', dot_product(u,v)

end program dot

