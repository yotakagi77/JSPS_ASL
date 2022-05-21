module subpro_mat_det
    implicit none
    contains
    recursive function func_det(a, n) result(det)
        integer, intent(in) :: n
        real(8), intent(in) :: a(n,n)
        real(8) det, b(n-1,n-1)
        integer j
        if (n > 1) then
            det = 0.0d0

            do j = 1, n
                b(1:n-1, 1:j-1) = a(2:n, 1:j-1)
                b(1:n-1, j:n-1) = a(2:n, j+1:n)
                det = det + ((- 1.0d0)**(1+j)) * a(1,j) * func_det(b, n-1)
            end do
        else
            det = a(1, 1)
        end if
        end function func_det
end module subpro_mat_det


program cal_mat
    use subpro_mat_det
    implicit none
    integer, parameter :: n = 5
    real(8) a(n,n)
    integer i

    call random_seed
    call random_number(a)
    write(*,*) 'matrix A'
    do i = 1, n
        write(*,*) a(i,:)
    end do
    write(*,*) '|A| = ', func_det(a, n)
end program cal_mat
