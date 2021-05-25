program ensyu15
implicit none

integer :: n(2),m,i,j
double precision,allocatable :: x(:,:,:)
open(1,file='p62_2.15_nakase.txt')

!分割数を決定
  write(*,*) "Input n(1:2)>=2"
  read(*,*) n(1),n(2)
  if(n(1)<2 .or. n(2)<2) stop 'error'
  
  allocate(x(2,n(1),n(2)))

!各座標の定義
  do j=1,n(2)
     do i=1,n(1)
	    x(1,i,j)=(dble(i)-1)/(dble(n(1))-1)
		x(2,i,j)=(dble(j)-1)/(dble(n(2))-1)
	 end do
  end do

!格子の出力
!縦線の出力
  do i=1,n(1)
     do j=1,n(2)
	    write(1,*) x(1:2,i,j)
	 end do
	 write(1,*) ""
  end do
!横線の出力
  do j=1,n(2)
     do i=1,n(1)
	    write(1,*) x(1:2,i,j)
	 end do
	 write(1,*) ""
  end do

stop
end program ensyu15