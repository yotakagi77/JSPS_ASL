program matmuluse
    implicit none
    integer, parameter :: n = 5
    integer i
    real(8) a(1:n, 1:n), b(1:n, 1:n), res1(1:n, 1:n), res2(1:n, 1:n)
    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))
    res1(1:n, 1:n) = a(1:n, 1:n) * b(1:n, 1:n)
    res2(1:n, 1:n) = matmul(a, b)
    write(*,*) 'a * b '
    do i = 1, n
        write(*,*) res1(i,:)
    end do
    write(*,*) 'matmul(a,b)'
    do i = 1, n
        write(*,*) res2(i,:)
    end do
end program matmuluse