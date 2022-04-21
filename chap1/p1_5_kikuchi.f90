program loop_inf
    implicit none
    integer wa, m, n, i
    do
        write(*,*) 'input m (if m <= 0, stop) : '
        read(*,*) m 
        write(*,*) 'input n (if n < m, stop) :'
        read(*,*) n 
        if (m <= 0) stop 'good bye ...'
        if (n < m) stop 'good bye ...'
        wa = 0
        do i = m, n
            wa = wa + i 
        end do
        write(*,*) 'wa = ', wa
    end do
end program loop_inf