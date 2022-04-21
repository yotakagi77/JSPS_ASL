program loop_err
    ! implicit none 宣言をしない
    integer i, wa
    wa = 0
    do i =1, 100
     wa = va + i ! 右辺のwaをvaと打ち間違えた
    end do
    write(*,*) 'wa = ', wa
   end program loop_err