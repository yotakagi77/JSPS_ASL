! リスト6.1　ガウスージョルダン法　モジュールサブルーチン
module g_j_subprogs
    implicit none
contains
    subroutine gauss_jordan(a0, x, b, n)
        integer, intent(in)  :: n
        real(8), intent(in)  :: a0(n,n), b(n)
        real(8), intent(out) :: x(n)
        integer i, k
        real(8) ar, a(n,n)
        a(:,:) = a0(:,:)
        x(:)   = b(:)
        do k = 1, n
            if(a(k,k) == 0) stop 'pivot = 0'
            ar = 1.0d0 / a(k,k)
            a(k,k)      = 1.0d0
            a(k, k+1:n) = ar * a(k, k+1:n)
            x(k)        = ar * x(k)
            do i = 1, n
                if (i /= k) then
                    a(i, k+1:n) = a(i, k+1:n) - a(i,k) * a(k, k+1:n)
                    x(i)        = x(i) - a(i,k) * x(k)
                    a(i,k)      = 0.0d0
                end if
            end do
        end do
    end subroutine gauss_jordan

    subroutine set_random_ab(a, b, x, n)
        integer, intent(in) :: n
        real(8), intent(out), allocatable :: a(:,:), b(:), x(:)
!        integer i
        allocate(a(n,n), b(n), x(n))
        call random_seed
        call random_number(a)
        call random_number(b)
!        write(*,*) 'MATRIX A'
!        do i = 1, n 
!            write(*,*) a(i,:)
!        end do
!        write(*,*) 'MATRIX B'
!        write(*,*) b(:)
    end subroutine set_random_ab
end module g_j_subprogs

! 演習6.1　ガウスの消去法　モジュールサブルーチン
module g_e_subprogs
    implicit none
contains
    subroutine gaussian_elimination(a0, x, b, n)
        ! --- ガウス消去法(部分 pivot 選択なし)---
        integer, intent(in) :: n
        real(8), intent(in) :: a0(n,n), b(n)
        real(8), intent(out) :: x(n)
        integer i, j, k
        real(8) ar, a(n,n), sigma
        a(:,:) = a0(:,:)
        x(:)   = b(:)
        ! 前進消去
        do k = 1, n
            if(a(k,k) == 0.0d0) stop 'pivot = 0'
            ar = 1.0d0 / a(k,k)
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
        ! 後退代入
        sigma = 0.0d0
        do i = n - 1, 1, -1
            do j = i + 1, n
                sigma = sigma + a(i, j) * x(j)
            end do
            x(i) = b(i) - sigma
        end do
    end subroutine gaussian_elimination
end module g_e_subprogs
   

program time_comp
    use g_j_subprogs
    use g_e_subprogs
    implicit none
    real(8), allocatable :: a(:,:), b(:), x(:)
    real(8) t1, t2, t3, t4
    integer n

    write(*,*) 'input n :'
    read(*,*) n

    call set_random_ab(a, b, x, n)
    ! リスト6.1　ガウスージョルダン法
    call cpu_time(t1)
    call gauss_jordan(a, x, b, n)
    call cpu_time(t2)
!    write(*,*) x(:)
    write(*,*) 'cpu time(G-J) :', t2 - t1
    deallocate(a, b, x)

    call set_random_ab(a, b, x, n)
    ! 演習6.1　ガウスの消去法　モジュールサブルーチン
    call cpu_time(t3)
    call gaussian_elimination(a, x, b, n)
    call cpu_time(t4)
!    write(*,*) x(:)
    write(*,*) 'cpu time(G-E) :', t4 - t3
    deallocate(a, b, x)

end program time_comp
