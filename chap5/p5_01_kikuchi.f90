! alpha 追加
module crossproduct
    implicit none
contains
    subroutine sub_cp(a, b, cp, alpha)
        real(8), intent(in), optional :: alpha
        real(8), intent(in) :: a(3), b(3)
        real(8), intent(out) :: cp(3)
        real(8) mag
        cp(1) = a(2) * b(3) - a(3) * b(2)
        cp(2) = a(3) * b(1) - a(1) * b(3)
        cp(3) = a(1) * b(2) - a(2) * b(1)
        mag = sqrt(dot_product(cp, cp))
        if (present(alpha)) then
            cp(:) = (cp(:) / mag) * alpha
        else
            cp(:) = cp(:) / mag
        end if
    end subroutine sub_cp
end module crossproduct


program axb
    use crossproduct
    implicit none
    real(8) a(3), b(3), cp(3), alpha
    integer i

    call random_seed
    call random_number(a)
    call random_number(b)
    write(*,*) 'vector a :', a(:)
    write(*,*) 'vector b :', b(:)

    write(*,*) 'input alpha ? (YES = 1, NO = 0)'
    read(*,*) i
    if (i == 1) then
        write(*,*) 'input alpha :'
        read(*,*) alpha
        call sub_cp(a, b, cp, alpha)
        write(*,*) 'a x b = ', cp
    else
        call sub_cp(a, b, cp)
        write(*,*) 'a x b = ', cp
    end if
end program axb
