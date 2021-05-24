program dispersion_relation
 implicit none
 
 double precision :: h,t
 
 write(*,*) "dispersion relation"
 write(*,*) "Input h,t"
 read(*,*) h,t
  
call bisection(h,t)
  
stop
contains



!サブルーチン"二分法"
subroutine bisection(h,t)
  integer :: i
  double precision :: eps,a,b,c,h,t
    dimension a(100),b(100),c(100)
!閾値
  eps=0.0001
!初期値入力
write(*,*) 'Input the range for bisection method'
read(*,*) a(1),b(1)
!初期値の有効性の確認
 do i=1,100
  if (f(a(1),h,t)*f(b(1),h,t)>=0) then
   write(*,*) 'Input in the another range'
   write(*,*) 'Please type in the assumption1'
   read(*,*) a(1),b(1)
  if (f(a(1),h,t)*f(b(1),h,t)<0) exit
  end if
 end do 
!二分法
 do i=1,100
  c(i)=(a(i)+b(i))/2
   if (b(i)-a(i).LT.eps) then
   write(*,'(a,f0.3)') 'the answer is ',c(i)
   stop
  else if (f(a(i),h,t)*f(c(i),h,t)<0) then
    b(i+1)=c(i)
    a(i+1)=a(i)
   else if (f(a(i),h,t)*f(c(i),h,t)>0) then
    a(i+1)=c(i)
    b(i+1)=b(i)
  end if
 end do
!収束しなかった場合
 write(*,*) 'the answer is not available'

 return
end



!関数定義 分散関係式
double precision function f(x,h,t)
 implicit none
 double precision :: h,t,g,x,pi
  pi=4.0*atan(1.0)
  g=9.81
  f=SQRT(g*x*TANH(2*pi*h/x)/(2*pi))-x/t
 return
end



end program dispersion_relation


