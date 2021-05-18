program en15
 implicit none
 integer wa, n, m, i
 do
   write(*,*) 'input m,n (if n<=m,stop):'
   read(*,*)m,n
   if (n<=m) stop 'good bye...'
   wa=0
   do i=m,n
     wa=wa+i
   end do
   write(*,*) 'wa=',wa
 end do
end program en15