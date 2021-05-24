program p1_8
 implicit none
 integer n,i
 write(*,*)'Input n'
 read(*,*)n
 if (n <= 0) then
  write(*,*)'stop, n <= 0'
  stop
 end if
 do i=2,n-1
  if (mod(n,i) == 0) then
   write(*,*)n,'is not prime number'
   stop
  end if
 end do
 write(*,*)n,' is prime number.'
 stop
end program p1_8
 