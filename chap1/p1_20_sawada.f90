program p1_20
 implicit none
 real(8) x,y,r,theta,pi
 integer i,n,fi1,fi2,fo
 open(fi1, file='input1_p1_20.d')
 open(fi2, file='input2_p1_20.d')
 open(fo, file='output_p1_20.d')
 read(fi1,*)r
 close(fi1)
 read(fi2,*)n
 close(fi2)
 pi=2.0d0*acos(0.0d0)
 do i=0,n
  theta=i*2*pi/n
  x=r*cos(theta)
  y=r*sin(theta)
  write(fo,*)x,y
 end do
 close(fo)
end program p1_20
 