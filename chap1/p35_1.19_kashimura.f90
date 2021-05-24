program p35_kashimura
 implicit none
  integer :: i,n
  double precision :: f1(255),f2(255),f3(255),x(255),h
  
!出力先ファイル
  open(1,file='p35_kashimura.txt')
  write(1,*) "#    x     "," ","  sinh(x)  "," ","  cosh(x)  "," ","  tanh(x)  "
  
!分割数の定義
  write(*,*) 'Input n<256(Division number)'
  read(*,*) n
  
  x(1)=-1
  h=2/(dble(n)-1)
  
  do i=1,n
	x(i+1)=x(i)+h
    f1(i)=sinh(x(i))
	f2(i)=cosh(x(i))
	f3(i)=tanh(x(i))
	write(1,2) x(i),f1(i),f2(i),f3(i)
  end do
  
  2 format(e11.4," ",e11.4," ",e11.4," ",e11.4)
  
stop
end program p35_kashimura