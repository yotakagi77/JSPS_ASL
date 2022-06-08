module subprog
    implicit none
contains
    function norm(u) result(u_norm)
        real(kind(1d0)),intent(in)::u(:)
        real(kind(1d0)) u_norm
        u_norm=sqrt(dot_product(u,u))
    end function norm

end module subprog

program name
    use subprog
    implicit none
    integer n
    real(kind(1d0)),allocatable:: u(:)

    write(*,*) "input n:"
    read(*,*) n
    allocate(u(n))

    call random_seed
    call random_number(u)
    write(*,*) "u(i)="
    write(*,*) u(:)
    write(*,*) "norm="
    write(*,*) norm(u)
end program name