program ensyu6
 implicit none
 integer wa, m, n, i
do
  write(*,*) ' input m and n (if m, n <= 0, stop) : '
  read(*,*) m, n
  if(m <= 0 .or. n <= 0 ) stop 'good bye...'
  wa =0

  if( m < n) then
  do i = m, n
   wa = wa + i
  end do
  write(*,*) 'wa =' , wa

  else if( m > n) then
  do i = n, m
   wa = wa + i
  end do
  write(*,*) 'wa =' , wa

  end if

end do
end program ensyu6