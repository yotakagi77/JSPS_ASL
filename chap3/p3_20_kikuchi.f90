module determine
    implicit none
contains
    function func_det(x) result(detx)
        real(8), intent(in) :: x(2, 2)
        real(8) detx
        detx = x(1,1) * x(2,2) - x(1,2) * x(2,1)
    end function func_det
end module determine

program det_of_mat
    use determine
    implicit none
    real(8) a(2,2), b(2,2), ab(2,2)
    real(8) deta, detb, detab
    call random_seed
    call random_number(a(:,:))
    call random_number(b(:,:))

    ab(:,:) = matmul(a, b)

    deta  = func_det(a(:,:))
    detb  = func_det(b(:,:))
    detab = func_det(ab(:,:))

    write(*,*) '|A| = ', deta
    write(*,*) '|B| = ', detb
    write(*,*) '|A||B| = ', deta * detb
    write(*,*) '|AB| = ', detab
end program det_of_mat