module identity_mat
    implicit none
contains
    function func_id_mat(n) result(a)
    real(8) a(n,n)
    integer, intent(in) :: n
    integer i, j
    do i = 1, n 
        do j = 1, n
            if (i == j) then
                a(i, j) = 1
            else
                a(i, j) = 0
            end if
        end do
    end do
    end function func_id_mat
end module identity_mat

program identity_matrix
    use identity_mat
    implicit none
    real(8), allocatable :: a(:,:)
    integer n, i

    write(*,*) 'input n(natural snumber) :'
    read(*,*) n
    allocate(a(n,n))

    a(:,:) = func_id_mat(n)

    do i = 1, n 
        write(*,*) a(i,:)
    end do

end program identity_matrix