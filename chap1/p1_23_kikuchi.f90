program elliptic_Integral
    implicit none
    integer n, i 
    real(8) pi, k, a_n, k_n, dif
    pi = 2.0d0 * acos(0.0d0)
    k_n = pi / 2
    a_n = 1.0d0
    write(*,*) 'input natural number n'
    read(*,*) n
    if (n < 1) stop 'stop, n < 1'
    write(*,*) 'input k (0 <= k < 1)'
    read(*,*) k 
    if (k <= 0) stop 'stop, k <= 0'
    if (k > 1) stop 'stop, k > 1'
    do i = 1, n-1 
        a_n = a_n * ((2*i-1)**2) / ((2*i)**2)
        k_n = k_n + (pi / 2) * a_n * ((2*k)**(2*i))
        if (i == n-2) then
            dif = k_n
        else if (i == n-1) then
            dif = k_n - dif 
        end if
    end do
    write(*,*) 'K(k) ~ ', k_n 
    write(*,*) 'e = ', dif
end program elliptic_Integral