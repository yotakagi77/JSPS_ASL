program p18_kashimura
 implicit none
  integer :: i,n

  write(*,*) "Input n"
  read(*,*) n

  i=2
  
  if (n>10000) then
   write(*,*) "error"
  else if (0>n) then
   write(*,*) "error"
  else 
	do
     if (i <= int(sqrt(dble(n)))) then	
	  if(mod(n,i)==0) then
	   n=n/i
	   write(*,*) i
	  else
	   i=i+1
	  end if
     else
	  exit
	 end if 
	end do
	write(*,*) n
  end if
  
 stop
end program p18_kashimura