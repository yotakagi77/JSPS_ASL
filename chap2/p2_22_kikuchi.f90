program matrix_33to22
    implicit none
    integer n, i, j, k, l
    real(8), allocatable :: a(:,:), b(:,:)
    n = 3
    allocate (a(n, n), b(n-1, n-1))
    call random_number(a(1:n, 1:n))
    do k = 1, 3
        write(*,*) (a(k, l), l = 1, 3)
    end do
    write(*,*) 'input i (1 or 2 or 3)'
    read(*,*) i 
    if (n <= 0) stop 'stop, i is wrong'
    write(*,*) 'input j (1 or 2 or 3)'
    read(*,*) j
    if (n <= 0) stop 'stop, j is wrong'
    do k = 1, n
        b(1:i-1, 1:j-1) = a(1:i-1, 1:j-1)
        b(1:i-1, j:n-1) = a(1:i-1, j+1:n)
        b(i:n-1, 1:j-1) = a(i+1:n, 1:j-1)
        b(i:n-1, j:n-1) = a(i+1:n, j+1:n)
    end do
    do k = 1, 2
        write(*,*) (b(k, l), l = 1, 2)
    end do
end program matrix_33to22