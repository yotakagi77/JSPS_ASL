module mat_subprogs
    implicit none
contains
    subroutine print_mat2(a)
        real(8), intent(in) :: a(:,:)
        integer i, n, m
        n = size(a, 1)
        m = size(a, 2)
        do i = 1, n
            write(*,*) a(i, 1:m)
        end do
    end subroutine print_mat2
end module mat_subprogs

program print_matrix
    use mat_subprogs
    implicit none
    real(8), allocatable :: a(:,:)
    allocate (a(-2:2, -3:3))
    call random_seed
    call random_number(a(:,:))
    call print_mat2(a(:,:))
end program print_matrix