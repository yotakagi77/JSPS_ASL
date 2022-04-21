program nepier
    implicit none
    integer n 
    real(8) x, e, e_n, fac, er
    x = 1.0d0
    e = exp(1.0d0)
    e_n = 1.0d0
    fac = 1.0d0
    do n = 1, 10
        fac = fac * n 
        e_n = e_n + ((x**n) / fac)
        write(*,*) 'n = ', n, 'e ~ ', e
    end do
    er = abs(e - e_n)
    write(*,*) 'er = ', er
end program nepier