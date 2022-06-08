! リスト3.2
module subprog
    implicit none
contains
    subroutine swap(a, b)
        integer, intent(inout) :: a, b
        integer tmp
        tmp = a
        a   = b
        b   = tmp
    end subroutine swap
end module subprog

! 演習3.2
module xplusy
    implicit none
contains
    subroutine plus(x, y, z)
        integer, intent(in) :: x, y
        integer, intent(out) :: z
        z = x + y
    end subroutine plus
end module xplusy

program use_intent
    use subprog
    use xplusy
    implicit none
    integer i, j, k
    i = 5
    j = 8
    call plus(i, j, k)
    write(*,*) 'i + j = ', k
    write(*,*) 'before'
    write(*,*) 'i = ', i, 'j = ', j
    call swap(i, j)
    write(*,*) 'after'
    write(*,*) 'i = ', i, 'j = ', j
end program use_intent