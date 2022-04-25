program loop_err
  implicit none
  integer i, wa
  wa = 0
  do i = 1, 100
     wa = wa + i ! 右辺のwaをvaと打ち間違えてしまった!!
  enddo
  write(*,*) 'wa = ', wa
end program loop_err