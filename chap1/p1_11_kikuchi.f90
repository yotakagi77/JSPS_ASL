program loop_odd_even2
    implicit none
    real(8) i, j, k, wa0, wa1
    wa0 = 0.0d0
    wa1 = 0.0d0
    do i = 1, 100
        j = i / 2
        k = int(i / 2)
        if (j == k) then
            wa0 = wa0 + i 
        else if  (j > k) then
            wa1 = wa1 + i 
        else
            stop 'something is wrong !!'
        end if
    end do
    write(*,*) 'wa0 = ', wa0
    write(*,*) 'wa1 = ', wa1
end program loop_odd_even2