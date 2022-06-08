program eq_true
    implicit none
    integer n, i, eq1_l, eq1_r, eq2_l, eq2_r, eq3_l, eq3_r
    n = 15
    eq1_l = 0
    eq2_l = 0
    eq3_l = 0
    eq1_r = (1/2) * n * (n+1)
    eq2_r = (1/6) * n * (n+1) *(2*n+1)
    eq3_r = (1/4) * (n**2) * ((n+1)**2)
    do i = 1, n
        eq1_l = eq1_l + i
        eq2_l = eq2_l + (i**2)
        eq3_l = eq3_l + (i**3)
    end do
    write(*,*) 'eq1_l = ', eq1_l, 'eq1_r = ', eq1_r
    write(*,*) 'eq2_l = ', eq2_l, 'eq2_r = ', eq2_r
    write(*,*) 'eq3_l = ', eq3_l, 'eq3_r = ', eq3_r
end program eq_true
