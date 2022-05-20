! 演習3.23より引用
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

! リスト3.25と3.28より引用
module GS_orthonormalization
    implicit none
contains
    function normal_vec2(v, n) result(nv)
        integer, intent(in) :: n 
        real(8), intent(in) :: v(n)
        real(8) nv(n), vl
        vl = sqrt(dot_product(v, v))
        if (vl == 0.0d0) then
            nv(:) = 0.0d0
        else
            nv(:) = v(:) / vl
        end if
    end function normal_vec2

    function gs(a, n) result(e)
        integer, intent(in) :: n
        real(8), intent(in) :: a(n, n)
        real(8) e(n, n), dotp
        integer k,j
        e(1:n, 1) = normal_vec2(a(1:n, 1:1), n)
        do k = 2, n 
            e(1:n, k) = a(1:n, k)
            do j = 1, k - 1
                dotp = dot_product(a(1:n, k), a(1:n, j))
                e(1:n, k) = e(1:n, k) - dotp * e(1:n, j)
            end do
            e(1:n, k) = normal_vec2(e(1:n, k:k), n)
        end do
    end function gs
end module GS_orthonormalization


program GS_orthonormalization_for3x3mat
    use det_a
    use GS_orthonormalization
    implicit none
    integer i
    real(8) a(3,3), t(3,3), adj_t(3,3), det_t

    call random_seed
    call random_number(a)
!    a(1,:) = (/1.0d0, 2.0d0, 3.0d0/)
!    a(2,:) = (/4.0d0, 5.0d0, 6.0d0/)
!    a(3,:) = (/7.0d0, 8.0d0, 8.0d0/)
    write(*,*) 'matrix A :'
    do i = 1, 3
        write(*,*) a(i,:)
    end do

    t(:,:) = gs(a, 3)
    write(*,*) 'matrix T :'
    do i = 1, 3
        write(*,*) t(i,:)
    end do

    adj_t(:,:) = func_adj(t)
    det_t = func_det(3, t, adj_t)
    write(*,*) '|T| = ', det_t
    
end program GS_orthonormalization_for3x3mat
