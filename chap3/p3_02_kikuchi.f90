module xplusy
    implicit none
contains
    subroutine plus(x, y, z)
        integer x, y, z
        z = x + y
    end subroutine plus
end module xplusy

program iplusj
    use xplusy
    implicit none
    integer :: i = 5, j = 8, k
    call plus(i, j, k)
    write(*,*) 'i + j = ', k
end program iplusj