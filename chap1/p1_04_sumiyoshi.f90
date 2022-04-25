program loop_do
  implicit none
  integer i
  do i = 1, 5, 2
    write(*,*) i
  enddo

  do i = 2, 8, 4
    write(*,*) i
  enddo

  do i = 3, 9, 2
    write(*,*) i
  enddo

end program loop_do

  