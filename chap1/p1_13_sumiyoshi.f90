program integnd

implicit none
integer :: n,i
double precision :: a,b,dh,sdai,ssim,f,x1,x2,xm
!
write(*,'(a,$)') 'a='
read(*,*) a
write(*,'(a,$)') 'b='
read(*,*) b
write(*,'(a,$)') 'N='
read(*,*) n
dh=(b-a)/real(n)
sdai=0.0
ssim=0.0
do i=1,n
x1=a+dh*float(i-1)
x2=a+dh*float(i)
xm=(x1+x2)/2.0
sdai=sdai+dh/2.0*(f(x1)+f(x2))
ssim=ssim+dh/6.0*(f(x1)+4.0*f(xm)+f(x2))
enddo
write(*,*) 'S.daikei =',sdai
write(*,*) 'S.simpson=',ssim
end program integnd
!
function f(x)
implicit none
double precision :: f,x,pi
pi=atan(1.0d+0)*4.0
f=1.0/sqrt(2.0*pi)*exp(-x**2/2.0)
end function f