subroutine gauss_jordan_pv2(a, n)
    ! --- 逆行列導出用ガウス-ジョルダン法（部分ピボット選択あり）---
    integer, intent(in) :: n
    real(8), intent(inout) :: a(n,2*n)  
    integer i, k, m
    real(8) ar, am, w(2*n)
    
    do k = 1, n
        m = k
        am = abs(a(k,k))

        do i = k+1, n
            if (abs(a(i,k)) > am) then
                am = abs(a(i,k))
                m = i
            end if
        end do

        if (am == 0.0d0) stop 'A is singular'

        if (k /= m) then
            w(   k:2*n) = a(k, k:2*n)
            a(k, k:2*n) = a(m, k:2*n)
            a(m, k:2*n) = w(   k:2*n)
        end if
        
        ar = 1.0d0 / a(k,k)
        a(k, k) = 1.0d0
        a(k, k+1:2*n) = ar * a(k, k+1:2*n)
        do i = 1, n
           if (i /= k) then
                a(i, k+1:2*n) = a(i, k+1:2*n) - a(i,k) * a(k, k+1:2*n)
                a(i, k) = 0.0d0
            end if
        end do
    end do
end subroutine gauss_jordan_pv2

! 演習3.28より引用
subroutine func_id_mat(n, a)
    integer, intent(in) :: n
    real(8), intent(out) :: a(n,n)
    integer i, j
    do i = 1, n 
        do j = 1, n
            if (i == j) then
                a(i, j) = 1.0d0
            else
                a(i, j) = 0.0d0
            end if
        end do
    end do
end subroutine func_id_mat

program nx2nmat_gj
    integer n, k
    real(8), allocatable :: a(:,:), i(:,:), ai(:,:)

    write(*,*) 'input n :'
    read(*,*) n
    allocate(a(n,n), i(n,n), ai(n,2*n))

    call random_seed
    call random_number(a)
    write(*,*) 'matrix A'
    do k = 1, n
        write(*,*) a(k,:)
    end do

    call func_id_mat(n, i)    
    write(*,*) 'matrix I'
    do k = 1, n
        write(*,*) i(k,:)
    end do

    do k = 1, n
        ai(k, 1:n) = a(k, 1:n)
        ai(k, n+1:2*n) = i(k, 1:n)
    end do

    call gauss_jordan_pv2(ai, n)
    write(*,*) 'matrix A^-1'
    do k = 1, n
        write(*,*) ai(k,:)
    end do

    deallocate(a, i, ai)

end program nx2nmat_gj

