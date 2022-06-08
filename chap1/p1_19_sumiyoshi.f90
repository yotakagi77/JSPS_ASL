program graph

implicit none

real(8) :: x, s, c, t, d

integer :: i, n

write(*,'(a,$)')  'input n'
read (*,*) n

d = 2/ dble(n-1)
 
do i = 1, n
  x = -1 + dble(i-1)*d
  s = sinh(x)
  c = cosh(x)
  t = tanh(x)

 write(*,*) x, s, c, t

 open(1, file = ' output1.dat')
 write(1,*) x, s

 open(2, file = ' output2.dat')
 write(2,*) x, c

 open(3, file = ' output3.dat')
 write(3,*) x, t
enddo





end program graph


