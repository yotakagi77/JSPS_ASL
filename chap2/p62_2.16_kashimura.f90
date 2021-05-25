program p62_kashimura
implicit none

integer :: n(2),m,i,j
double precision :: pi
double precision,allocatable :: x(:,:,:),phi(:,:)
open(1,file='p62_2.16_kashimura.txt')

pi=4.0*atan(1.0)

!分割数を決定
  write(*,*) "Input n(1:2)>=2"
  read(*,*) n(1),n(2)
  if(n(1)<2 .or. n(2)<2) stop 'error'
  
  allocate(x(2,n(1),n(2)))
  allocate(phi(n(1),n(2)))

!各座標の定義
  do j=1,n(2)
     do i=1,n(1)
	    x(1,i,j)=(dble(i)-1)/(dble(n(1))-1)
		x(2,i,j)=(dble(j)-1)/(dble(n(2))-1)
		phi(i,j)=sin(pi*x(1,i,j))*sinh(pi*(1-x(2,i,j)))/sinh(pi)
	 end do
  end do

!格子の出力
  do i=1,n(1)
     do j=1,n(2)
	    write(1,*) x(1:2,i,j),phi(i,j)
	 end do
	 write(1,*) ""
  end do


stop
end program p62_kashimura