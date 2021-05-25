program p43_kashimura
 implicit none
 
 integer :: i
 double precision :: u(5),v(5),w(3)

 !行列の読み込み
 write(*,*) "Input u(3)"
  read(*,*) u(1),u(2),u(3)
 write(*,*) "Input v(3)"
  read(*,*) v(1),v(2),v(3)
  
  u(4)=u(1)
  v(4)=v(1)
  u(5)=u(2)
  v(5)=v(2)
  
!外積の演算
 do i=1,3
  w(i)=u(i+1)*v(i+2)-u(i+2)*v(i+1)
 end do
 write(*,*) w(1),w(2),w(3)
 
stop
end program p43_kashimura