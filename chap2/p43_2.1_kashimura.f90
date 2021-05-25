program p43_kashimura
 implicit none
 
 integer :: i
 double precision :: u(3),v(3),x

 open(1,file='mat.txt')

 write(*,*) "u(i) v(i)"
 
 do i=1,3
  read(1,*) u(i),v(i)
  write(*,*) u(i),v(i)
 end do
 
 x=0
 
 do i=1,3
  x=x+u(i)*v(i)
 end do
 
 write(*,*)  " inner product "
 write(*,*) x

 close(1)
 
stop
end program p43_kashimura