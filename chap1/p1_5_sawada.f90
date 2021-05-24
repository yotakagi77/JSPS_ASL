program p1_5
 implicit none
 integer wa,m,n,i
 do
  write(*,*) 'Input m and n (if m>n, stop)'
  read(*,*) m,n
  if (m > n) stop 'good bye...'
  wa=m-1
  do i =1,n
   wa=wa+i
  end do
  write(*,*) 'wa=',wa
 end do
end program p1_5