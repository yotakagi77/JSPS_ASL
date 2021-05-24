program p1_23
 implicit none
 real(8) k,num1,num2,den1,den2,x1,x2,s1,s2,ans1,ans2,e
 integer pi,n,i,j
 write(*,*) 'Input k (0 <= k^2 < 1)'
 read(*,*) k
 if (k**2 < 0) then
  stop
  else if (k**2 >= 1) then
  stop
 end if
 write(*,*) 'Input n (n >= 2)'
 read(*,*) n
 pi=2.0d0*acos(0.0d0)
 num1=1
 den1=1
 s1=1
 do i=2,n
  num1=num1*(2*i-3)**2
  den1=den1*(2*i-2)**2
  x1=k**(2*i-2)
  s1=s1+x1*(num1/den1)
  ans1=s1*pi/2
 end do
 num2=1
 den2=1
 s2=1
 do j=2,n-1
  num2=num2*(2*j-3)**2
  den2=den2*(2*j-2)**2
  x2=k**(2*j-2)
  s2=s2+x2*(num2/den2)
  ans2=s2*pi/2
 end do
 e=abs(ans1-ans2)
 write(*,*)'K=',ans1,'|e|=',e
end program p1_23
