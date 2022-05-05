program simple_sort
    implicit none
    integer, parameter :: n = 10
    real(8) a(n), am, an
    integer i, j
    call random_seed
    call random_number(a(:))
    do i = 1, n - 1
        am = a(i)
        do j = i + 1, n 
            if (a(j) < am) then
                an = a(j)
                a(j) = am
                am = an
            end if
        end do
        a(i) = am
    end do
    write(*,*) a(:)
end program simple_sort