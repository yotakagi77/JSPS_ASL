!　ガウスの消去法(部分 pivot 選択あり)　モジュールサブルーチン
module g_e_subprogs2
    implicit none
contains
    subroutine gaussian_elimination_pv(a0, x, b, n, det)
        integer, intent(in)  :: n
        real(8), intent(in)  :: a0(n,n), b(n)
        real(8), intent(out) :: x(n)
        integer i, j, k
        real(8) ar, aj, a(n,n), t, w(n), sigma, alpha, det

        a(:,:) = a0(:,:)
        x(:)   = b(:)            
        alpha = 1.0d0

        do k = 1, n
            ! 部分 pivot 選択
            j = k
            aj = abs (a(k, k))
            do i = k + 1, n
                if (abs(a(i, k)) > aj) then
                    aj = abs(a(i, k))
                    j = i
                end if
            end do
            if (aj == 0.0d0) stop 'A is singular'
            if (k /= j) then
                w(   k:n) = a(k, k:n)
                a(k, k:n) = a(j, k:n)
                a(j, k:n) = w(   k:n)
                t    = x(k)
                x(k) = x(j)
                x(j) = t
                alpha = alpha * (- 1.0d0)
            end if

            ! 以下　演習6.2 より引用
            ar = 1.0d0 / a(k,k)
            alpha = alpha * a(k,k)
            a(k, k) = 1.0d0
            a(k, k+1:n) = ar * a(k, k+1:n)
            x(k) = ar * x(k)
            do i = 1, n
                if(i/=k) then
                    a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
                    x(i) = x(i) - a(i, k) * x(k)
                    a(i, k) = 0.0d0
                endif
            enddo
        enddo

        sigma = 0.0d0
        do i = n - 1, 1, -1
            do j = i + 1, n
                sigma = sigma + a(i, j) * x(j)
            end do
            x(i) = b(i) - sigma
        end do

        det = 1.0d0
        do i = 1, n
            det = det * a(i, i)
        end do
        det = det * alpha

    end subroutine gaussian_elimination_pv

    subroutine set_random_ab(a, b, x, n)
        integer, intent(in) :: n
        real(8), intent(out), allocatable :: a(:,:), b(:), x(:)
        integer i
        allocate(a(n,n), b(n), x(n))
        call random_seed
        call random_number(a)
        call random_number(b)
        write(*,*) 'MATRIX A'
        do i = 1, n 
            write(*,*) a(i,:)
        end do
        write(*,*) 'MATRIX B'
        write(*,*) b(:)
    end subroutine set_random_ab

end module g_e_subprogs2
   

program time_comp
    use g_e_subprogs2
    implicit none
    real(8), allocatable :: a(:,:), b(:), x(:)
    real(8) det
    integer n

    write(*,*) 'input n :'
    read(*,*) n

    call set_random_ab(a, b, x, n)
    call gaussian_elimination_pv(a, x, b, n, det)
    write(*,*) x(:)
    write(*,*) '|A| = ', det
    deallocate(a, b, x)

end program time_comp
