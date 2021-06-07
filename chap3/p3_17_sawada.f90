module subprogs
 implicit none
contains
 subroutine kaiten(a,b,c)
 real(8), intent(in)::a(:),c
 real(8), intent(out)::b(:)
 b(1)=a(1)*cos(c)-a(2)*sin(c)  !加法定理
 b(2)=a(2)*cos(c)+a(1)*sin(c)
 write(*,*) 'y(y1,y2)=',b(:)
 end subroutine kaiten
end module subprogs

program p3_17
 use subprogs
 implicit none
 real(8) x(1:2),y(1:2),theta
 integer fo
 open(fo,file='output_p3_17.d')
 write(*,*) 'Input x1,x2'
 read(*,*) x(1:2)
 write(*,*) 'Input theta'
 read(*,*) theta
 call kaiten(x,y,theta)
 write(fo,*) 0,0           !原点とx,yの三角形を描くため
 write(fo,*) x(1),x(2)
 write(fo,*) y(1),y(2)
 write(fo,*) 0,0
 close(fo)
end program p3_17
