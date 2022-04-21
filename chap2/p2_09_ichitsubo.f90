program name
    implicit none
    real(8) ,allocatable :: u(:),v(:)
    integer :: n
    open(17,file='input2_9.dat')
    open(18,file='output2_9.dat')
    read(17,*) n
    allocate(u(n),v(n))
    read(17,*) u(1:n)
    read(17,*) v(1:n)
    write(18,*) "dp = ",dot_product(u,v)
    deallocate (u,v)

end program name