subroutine set_bdc(n1, n2, x1, phi)
    implicit none
    integer, intent(in)  :: n1, n2
    real(8), intent(in)  :: x1(n1)
    real(8), intent(out) :: phi(n1, n2)

    phi(:,    1) = sin(2 * acos(0.0d0) * x1(:))
    phi(:, 2:n2) = 0.0d0

end subroutine set_bdc

subroutine chk_err(phi, c, d, n1, n2, er)
    implicit none
    integer, intent(in)  :: n1, n2
    real(8), intent(in)  :: phi(n1, n2), c, d
    real(8), intent(out) :: er
    integer i, j
    real(8) rhs

    er = 0.0d0
    do i = 2, n1 - 1
        do j = 2, n2 - 1
            rhs = -c * (phi(i-1,j  ) + phi(i+1,j  ))  &
                  -d * (phi(i  ,j-1) + phi(i  ,j+1))
            er = er + (rhs - phi(i,j))**2.0d0
        end do
    end do

end subroutine chk_err


program sor_itr_check
    implicit none
    real(8) dx1, dx2, c, d, rhs, er, er0, omg0, omg1, omg2, omg3
    real(8), allocatable :: phi(:,:), x1(:), x2(:)
    integer i, j, itr, itrmax, n1, n2

    ! 格子数 n1 = n2
    n1 = 101
    n2 = n1
    dx1 = 1.0d0 / dble(n1-1)
    dx2 = 1.0d0 / dble(n2-1)
    allocate(phi(n1,n2), x1(n1), x2(n2))

    c = (-dx2**2.0d0) / ((dx1**2.0d0) + (dx2**2.0d0)) / 2.0d0
    d = c * ((dx1 / dx2)**2.0d0)

    do i = 1, n1
        x1(i) = 0.0d0 + dx1 * dble(i-1)
    end do

    do j = 1, n2
        x2(j) = 0.0d0 + dx2 * dble(j-1)
    end do

    call set_bdc(n1, n2, x1, phi)

    itrmax = 10**5
    er0 = 1.0d-7

    ! 加速パラメータ 
    omg0 = 2.0d0 / (1 + sin(2*acos(0.0d0) / dble(n1 - 1)))
    omg1 = 1.0d0
    omg2 = 2.0d0
    omg3 = 3.0d0

    ! omega = omega_0
    write(*,*) 'omega_0'
    do itr = 1, itrmax
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = -c * (phi(i-1, j  ) + phi(i+1, j  ))  &
                      -d * (phi(i  , j-1) + phi(i  , j+1))
                phi(i, j) = phi(i, j) + omg0 * (rhs - phi(i, j))
            end do
        end do
        call chk_err(phi, c, d, n1, n2, er)
        if (er < er0) then
            write(*,*) "itr, er=", itr, er
                exit
        else if (itr == itrmax) then
            write(*,*) 'finish calculation'    
        end if
    end do

    ! omega = omega_1
    write(*,*) 'omega_1'
    do itr = 1, itrmax
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = -c * (phi(i-1, j  ) + phi(i+1, j  ))  &
                      -d * (phi(i  , j-1) + phi(i  , j+1))
                phi(i, j) = phi(i, j) + omg1 * (rhs - phi(i, j))
            end do
        end do
        call chk_err(phi, c, d, n1, n2, er)
        if (er < er0) then
            write(*,*) "itr, er=", itr, er
                exit
        else if (itr == itrmax) then
            write(*,*) 'finish calculation'
        end if
    end do

    ! omega = omega_2
    write(*,*) 'omega_2'
    do itr = 1, itrmax
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = -c * (phi(i-1, j  ) + phi(i+1, j  ))  &
                      -d * (phi(i  , j-1) + phi(i  , j+1))
                phi(i, j) = phi(i, j) + omg2 * (rhs - phi(i, j))
            end do
        end do
        call chk_err(phi, c, d, n1, n2, er)
        if (er < er0) then
            write(*,*) "itr, er=", itr, er
                exit
        else if (itr == itrmax) then
                write(*,*) 'finish calculation'
        end if
    end do

    ! omega = omega_3
    write(*,*) 'omega_3'
    do itr = 1, itrmax
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = -c * (phi(i-1, j  ) + phi(i+1, j  ))  &
                      -d * (phi(i  , j-1) + phi(i  , j+1))
                phi(i, j) = phi(i, j) + omg3 * (rhs - phi(i, j))
            end do
        end do
        call chk_err(phi, c, d, n1, n2, er)
        if (er < er0) then
            write(*,*) "itr, er=", itr, er
                exit
        else if (itr == itrmax) then
            write(*,*) 'finish calculation'
        end if
    end do

end program sor_itr_check