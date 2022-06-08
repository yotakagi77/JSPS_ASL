program vector
implicit none
real u(3),v(3),dot
integer i
open(10, file='mat.d')

read(10,*) u(1)
read(10,*) u(2)
read(10,*) u(3)
read(10,*) v(1)
read(10,*) v(2)
read(10,*) v(3)

write(*,*) (u(i), i = 1, 3)
write(*,*) (v(i), i = 1, 3)

dot = 0

do i = 1,3

dot = dot + u(i) * v(i)

enddo

write(*,*) 'ans = ' , dot

end program vector