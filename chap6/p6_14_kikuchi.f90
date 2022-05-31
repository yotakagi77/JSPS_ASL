module gs_bicgsrab
    implicit none
contains
    subroutine bicgstab1d(a, b, x, n, itrmax, er0)
        ! n はx の要素数、itrmax は最大反復数、er0 は収束判定の閾値
        integer, intent(in)  :: n, itrmax
        real(8), intent(in)  :: a(n, n), b(n), er0
        real(8), intent(out) :: x(n)
        integer itr
        real(8) alp, bet, c1, c2, c3, ev, vv, rr
        real(8) r(n), r0(n), p(n), y(n), e(n), v(n)

        x(:) = 0.0d0
        r(:) = b - matmul(a, x)
        c1 = dot_product(r, r)

        if (c1 < er0) return

        p(:) = r(:)
        r0(:) = r(:)

        do itr = 1, itrmax
            y(:) = matmul(a, p)
            c2 = dot_product(r0, y)
            alp = c1 / c2 
            e(:) = r(:) - alp * y(:)
            v(:) = matmul(a, e)
            ev = dot_product(e, v)
            vv = dot_product(v, v)
            c3 = ev / vv
            x(:) = x(:) + alp * p(:) + c3 * e(:)
            r(:) = e(:) - c3 * v(:)
            rr = dot_product(r, r)
            write(*,*) 'itr = ', itr, 'er = ', rr
            if (rr < er0) exit
            c1 = dot_product(r0, r)
            bet = c1 / (c2 * c3)
            p(:) = r(:) + bet * (p(:) - c3 * y(:))
        end do
    end subroutine bicgstab1d
    
    subroutine alloc_dd_mat(a, b, x, n)
        ! a、b、x を割つけ　→　a：狭義の対角有意行列　b：乱数　設定
        integer, intent(in) :: n 
        real(8), intent(out), allocatable :: a(:,:), b(:), x(:)
        integer i, j

        allocate(a(n,n), b(n), x(n))

        do i = 1, n
            do j = 1, n
                if (i == j) then
                    a(i, j) = 1.0d0
                else
                    call random_number(a(i, j))
                end if
            end do
        end do

        call random_number(b)
    end subroutine alloc_dd_mat

end module gs_bicgsrab


program gauss_seidel_bicgstab
    use gs_bicgsrab
    implicit none
    real(8), allocatable :: a(:,:), b(:), x(:)
    integer :: n, itrmax = 100
    real(8) :: er0 = 1.0d-6

    write(*,*) 'input n :'
    read(*,*) n
    
    call alloc_dd_mat(a, b, x, n)
    call bicgstab1d(a, b, x, n, itrmax, er0)
    write(*,*) 'answer x =', x(:)
end program gauss_seidel_bicgstab
