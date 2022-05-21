module crossproduct
    implicit none
contains
    subroutine sub_cp(a, b, cp)
        real(8), intent(in), optional :: a(3)
        real(8), intent(in) :: b(3)
        real(8), intent(out) :: cp(3)
        real(8) det_b
        if (present(a)) then
            cp(1) = a(2) * b(3) - a(3) * b(2)
            cp(2) = a(3) * b(1) - a(1) * b(3)
            cp(3) = a(1) * b(2) - a(2) * b(1)
        else
            det_b = sqrt(dot_product(b,b))
            cp(:) = (/ b(1)/det_b , b(2)/det_b , b(3)/det_b /)
        end if
    end subroutine sub_cp
end module crossproduct


program axb
    use crossproduct
    implicit none
    real(8) a(3), b(3), cp(3)

    call random_seed
    call random_number(a)
    call random_number(b)
    write(*,*) 'vector a :', a(:)
    write(*,*) 'vector b :', b(:)

    call sub_cp(a, b, cp)
    write(*,*) 'present a'
    write(*,*) 'cp = ', cp

    call sub_cp(b = b, cp = cp)
    write(*,*) 'else'
    write(*,*) 'cp = ', cp
end program axb
