program p24_kashimura
 implicit none
  integer :: i,n
  double precision :: f,a,b,h,P,x(3)
  
  write(*,*) "Integral range of normal distribution [a,b]"
  write(*,*) "Input a,b"
  read(*,*) a,b
  
!分割数
  write(*,*) 'Input n(Division number)'
  read(*,*) n
  h=(b-a)/n
 
!台形則
  P=0
  x(2)=a
  do i=1,n
   x(1)=x(2)
   x(2)=x(1)+h
   P=P+(f(x(1))+f(x(2)))*h
  end do
  P=P/2
  write(*,'(A,f6.3)') 'P=',P
 
!シンプソン積分
!  P=0
!  x(3)=a
!  do i=1,n/2
!   x(1)=x(3)
!   x(2)=x(1)+h
!   x(3)=x(2)+h
!   P=P+(f(x(1))+4*f(x(2))+f(x(3)))
!  end do
!  P=h/3*P
!  write(*,'(A,f6.3)') 'P=',P
  
stop
end program p24_kashimura

!関数定義 標準正規分布確率密度関数
real(8) function f(x)
implicit none
double precision pi,x
  pi=4.0*atan(1.0)
  f=1/(sqrt(2*pi))*exp(-x**2/2)
return
end