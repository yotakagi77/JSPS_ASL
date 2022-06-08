module det_a
    implicit none
contains
    function func_adj(a) result(adj)
        integer i, j
        real(8), intent(in) :: a(3,3)
        real(8) adj(3,3), b(3,3)
        do i = 1, 3
            do j = 1, 3
                b = cshift(a,i,1)
                b = cshift(b,j,2)
                adj(i,j) = (b(1,1) * b(2,2) - b(1,2) * b(2,1))
            end do
        end do
    end function func_adj

    function func_det(i, a, adj) result(det)
        integer, intent(in) :: i
        integer j
        real(8), intent(in) :: a(3,3), adj(3,3)
        real(8) det
        det = 0.0d0
        do j = 1,3
                det = det + a(i,j) * adj(i,j)
        end do
    end function func_det
end module det_a

program det_mat33
    use det_a
    implicit none
    integer i
    real(8) a(3,3), adj(3,3), det(3)

    call random_seed
    call random_number(a(:,:))

    write(*,*) "A ="
    do i = 1, 3
        write(*,*) a(i,:)
    end do

    adj(:,:) = func_adj(a)

    do i = 1, 3
        det(i) = func_det(i, a, adj)
        write(*,*) 'i = ', i
        write(*,*) "|A|=", det(i)
    end do

end program det_mat33