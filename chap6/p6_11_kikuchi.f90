module gsmethod
    implicit none
contains
    subroutine gauss_seidel(a, b, x, n, itrmax, er0)
        ! a = 係数行列　b = 右辺ベクトル　itrmax = 最大反復回数　er0 = 誤差のしきい値
        integer, intent(in)  :: n, itrmax
        real(8), intent(in)  :: a(n, n), b(n), er0 
        real(8), intent(out) :: x(n)
        real(8) s, er, rd(n), r(n)
        integer i, itr
        do i = 1, n
            if (a(i, i) == 0.0d0) stop 'a(i, i) == 0.0d0'
            rd(i) = 1.0d0 / a(i, i)
        end do
        do itr = 1, itrmax
            do i = 1, n
                s =     dot_product(a(1, 1  :i-1), x(1  :i-1))
                s = s + dot_product(a(1, i+1:n  ), x(i+1:n  ))
                x(i) = rd(i) * (b(i)- s)
            end do
            r(1:n) = b(1:n) - matmul(a, x)
            er = dot_product(r, r)
            write(*,*) 'itr = ', itr, 'err = ', er
            if (er <= er0) then
                write(*,*) '# converged #'
                exit
            end if
        end do
    end subroutine gauss_seidel
    
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

end module gsmethod

program gauss_seidel_method
    use gsmethod
    implicit none
    real(8), allocatable :: a(:,:), b(:), x(:)
    integer :: n, i, itrmax = 100
    real(8) :: er0 = 1.0d-6

    write(*,*) 'input n :'
    read(*,*) n

    call alloc_dd_mat(a, b, x, n)
    call gauss_seidel(a, b, x, n, itrmax, er0)

    write(*,*) 'matrix A'
    do i = 1, n
        write(*,*) a(i, n)
    end do

    write(*,*) 'matrix b : ', b(:)

    write(*,*) 'answer x : ', x(:)
end program gauss_seidel_method
