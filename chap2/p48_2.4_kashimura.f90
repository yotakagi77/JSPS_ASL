program p43_kashimura
 implicit none
 
 integer :: i
 double precision :: u(3),x

 !行列の読み込み
 write(*,*) "Input u(3)"
  read(*,*) u(1),u(2),u(3)
 
 write(*,*) "non-dot_product"
 x=0
 do i=1,3
  x=x+u(i)**2
 end do
 x=SQRT(x)
 write(*,*) x
 
 write(*,*) "dot_product"
 x=dot_product(u,u)
 x=SQRT(x)
 write(*,*) x
 
stop
end program p43_kashimura