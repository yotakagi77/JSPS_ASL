program random0to9
    implicit none
    integer n 
    real(8), allocatable :: a(:)
    write(*,*) 'input n(>= 1) :'
    read(*,*) n
    if (n < 1) stop 'stop, n < 1'
    allocate (a(n))
    call random_seed
    call random_number(a(1:n))
    a(1:n) = int(10 * a(1:n))
    write(*,*) a(1:n)
end program random0to9
