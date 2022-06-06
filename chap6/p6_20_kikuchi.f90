! ディリクレ境界条件
subroutine set_dbc(n1, n2, x1, phi)
    implicit none
    integer, intent(in)  :: n1, n2
    real(8), intent(in)  :: x1(n1)
    real(8), intent(out) :: phi(n1, n2)

    phi(:,    1) = sin(2 * acos(0.0d0) * x1(:))
    phi(:, 2:n2) = 0.0d0

end subroutine set_dbc

! ノイマン境界条件
subroutine set_nbc(n1, n2, phi)
    implicit none
    integer, intent(in)  :: n1, n2
    real(8), intent(out) :: phi(n1, n2)
    integer i

    do i = 1, n1
        phi(i, n2) = phi(i, n2-1)
    end do

end subroutine set_nbc

! ラプラス方程式用誤差判定
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

! 拡散方程式用定常性判定(各要素の差の２乗和)
subroutine chk_steady(n1, n2, phi, phi2, er)
    implicit none
    integer, intent(in)  :: n1, n2
    real(8), intent(in)  :: phi(n1, n2), phi2(n1, n2)
    real(8), intent(out) :: er
    integer i, j

    er = 0.0d0
    do i = 1, n1
        do j = 1, n2
            er = er + (phi(i, j) - phi2(i, j)) ** 2
        end do
    end do

end subroutine chk_steady

program lap_dif_comp
    implicit none
    real(8) dx1, dx2, c, d, rhs, er, er0, omg0
    real(8) dt, d1, d2
    real(8), allocatable :: phi(:,:), phi2(:,:), x1(:), x2(:)
    integer i, j, itr, itrmax, n1, n2, istep, nstep

    ! 共通
    n1 = 101
    n2 = n1
    dx1 = 1.0d0 / dble(n1-1)
    dx2 = 1.0d0 / dble(n2-1)
    allocate(phi(n1,n2), phi2(n1,n2), x1(n1), x2(n2))

    do i = 1, n1
        x1(i) = 0.0d0 + dx1 * dble(i-1)
    end do

    do j = 1, n2
        x2(j) = 0.0d0 + dx2 * dble(j-1)
    end do


    ! 以下、ラプラス方程式
    call set_dbc(n1, n2, x1, phi)
    c = (-dx2**2.0d0) / ((dx1**2.0d0) + (dx2**2.0d0)) / 2.0d0
    d = c * ((dx1 / dx2)**2.0d0)

    itrmax = 10**5
    er0 = 1.0d-7
    omg0 = 2.0d0 / (1 + sin(2*acos(0.0d0) / dble(n1 - 1)))

    do itr = 1, itrmax
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                rhs = -c * (phi(i-1, j  ) + phi(i+1, j  ))  &
                      -d * (phi(i  , j-1) + phi(i  , j+1))
                phi(i, j) = phi(i, j) + omg0 * (rhs - phi(i, j))
            end do
        end do
        phi(:, n2) = phi(:, n2 - 1)
        call chk_err(phi, c, d, n1, n2, er)
        if (er < er0) then
            write(*,*) 'itr, er=', itr, er
                exit
        else if (itr == itrmax) then
            write(*,*) 'finish calculation (Lap)'    
        end if
    end do

    open(50, file = "lap_phi.d")
    do j = 1, n2
        do i = 1, n1
            write(50,"(3e12.4)") x1(i), x2(j), phi(i, j)
        end do
        write(50,*) ''
    end do
    close(50)


    ! 以下、拡散方程式
    dt = 1.0d-5
    d1 = 2.5d-1
    d2 = 2.5d-1
    nstep = 10**3
    call set_dbc(n1, n2, x1, phi)
    call set_nbc(n1, n2, phi)
    do istep = 1, nstep
        do j = 2, n2 - 1
            do i = 2, n1 - 1
                phi2(i, j) = phi(i, j) &
                    + d1 * (phi(i-1, j  ) - 2.0d0 * phi(i, j) + phi(i+1, j  )) &
                    + d2 * (phi(i  , j-1) - 2.0d0 * phi(i, j) + phi(i  , j+1))
            end do
        end do
        call set_nbc(n1, n2, phi2)
        call chk_steady(n1, n2, phi, phi2, er)
        phi(:,:) = phi2(:,:)
        if (er < er0) then
            write(*,*) 'itr, er=', itr, er
                exit
        else if (itr == itrmax) then
            write(*,*) 'finish calculation (Dif)'    
        end if
    end do

    open(51, file = "dif_phi.d")
    do j = 1, n2
        do i = 1, n1
            write(50,"(3e12.4)") x1(i), x2(j), phi(i, j)
        end do
        write(51,*) ''
    end do
    close(51)

    deallocate(phi, phi2, x1, x2)
    write(*,*) 'end program, check files'

end program lap_dif_comp