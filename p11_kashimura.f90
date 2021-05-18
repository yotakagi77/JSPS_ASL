program p11_kashimura
 implicit none

  integer :: n,i,k1,k2
  
  k1=0
  k2=0
  
  i=0
   do i=1,100
    k1=k1+i
   end do
   
   n=100
   k2=n*(n+1)/2
 
  write(*,*) k1,k2

 stop
end program p11_kashimura