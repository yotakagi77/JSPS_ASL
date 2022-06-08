program regularization
    implicit none
    integer i 
    real(8) u(1:3), absu
    write(*,*) 'input vector u = (u1, u2, u3)'
    do i = 1, 3
        write(*,*) 'u', i, '='
        read(*,*) u(i)
    end do
    absu = sqrt(dot_product(u, u))
    if (absu == 0) stop 'stop, |u| = 0'
    do i = 1, 3
        u(i) = u(i) / absu
    end do
    write(*,*) 'eu = ', u(1:3)
end program regularization
