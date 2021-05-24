program p36_kashimura
 implicit none
 
  integer :: i
  double precision :: a,b,c,d,x1,x2
  
  !初期値設定
  write(*,*) 'ax^2+bx+c=0'
  write(*,*) 'Input a,b,c'
  read(*,*) a,b,c
  
  !判別式
  d=b**2-4*a*c
  
  if(d>0) then
   x1=(-b+SQRT(d))/(2*a)
   x2=(-b-SQRT(d))/(2*a)
   write(*,*) "x=",x1,",",x2
  else if(d==0) then
   x1=-b/(2*a)
   write(*,*) "x=",x1
  else
   x1=-b/(2*a)
   x2=SQRT(ABS(d))/(2*a)
   write(*,*) "x=",x1,"+-i",x2
  end if
  
stop
end program p36_kashimura