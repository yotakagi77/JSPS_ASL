!p86 演習3.10
module subprograms
  implicit none
contains

!第一種楕円積分の近似
  subroutine elliptic_integral(k,s)
    double precision,intent(in) :: k
	double precision,intent(out) :: s
	integer :: i,n
	double precision :: num,den,pi
	!num=分子,den=分母
    !項数の設定
    write(*,*) 'Input term number "n"'
    read(*,*) n
	!近似計算
	pi=4.0d0*atan(1.0d0)
	num=1.0d0
	den=1.0d0
	s=pi/2
	do i=2,n
	   num=num*((2*(i-1)-1)**2) !計算順序注意
	   den=den*((2*(i-1))**2)
	   s=s+pi/2*num/den*k**(2*(i-1))
	end do
  end subroutine elliptic_integral
  
end module subprograms



program p3_10_kashimura
  use subprograms
  implicit none

  double precision :: k,s
  
	write(*,*) 'modulus "k"'
    write(*,*) "Input 0<=k<1"
    read(*,*) k
	
	call elliptic_integral(k,s)
      write(*,*) "by Elliptic Integral"
      write(*,*) 'S=',s

stop
end program p3_10_kashimura