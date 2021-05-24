program p17_kashimura
 implicit none
  integer :: i,p,c,n,r
  write(*,*) "Input n,r"
  read(*,*) n, r

  if (n>10) then
   write(*,*) "error"
  else if (r>n) then
   write(*,*) "error"
  else 
   p=1
   c=1
    do i=1,r
	 p=p*(n-i+1)
	 c=c*(n-i+1)/i
    end do  
   write(*,*) "nPr=", p, "nCr=", c
  end if
 
 stop
end program p17_kashimura