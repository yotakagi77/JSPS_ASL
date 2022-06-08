module identity_mat
    implicit none
contains
    function func_id_mat(n) result(a)
    real(8) a(n,n)
    integer, intent(in) :: n
    integer s, t
    do s = 1, n 
        do t = 1, n
            if (s == t) then
                a(s,t) = 1.0d0
            else
                a(s,t) = 0.0d0
            end if
        end do
    end do
    end function func_id_mat
end module identity_mat


module e_trans 
    implicit none
contains
    function func_p(c, e, i, k) result(p)
        integer, intent(in) :: i, k
        real(8), intent(in) :: c, e(k,k)
        real(8) p(k,k)
        p(:,:) = e(:,:)
        p(i,i) = c
    end function func_p

    function func_q(c, e, i, j, k) result(q)
        integer, intent(in) :: i, j, k
        real(8), intent(in) :: c, e(k,k)
        real(8) q(k,k)
        q(:,:) = e(:,:)
        q(j,i) = c
    end function func_q

    function func_r(e, i, j, k) result(r)
        integer, intent(in) :: i, j, k
        real(8), intent(in) :: e(k,k)
        real(8) r(k,k)
        r(:,:) = e(:,:)
        r(i,i) = 0.0d0
        r(j,j) = 0.0d0
        r(i,j) = 1.0d0
        r(j,i) = 1.0d0
    end function func_r
end module e_trans


program elemenatary_transformation
    use identity_mat
    use e_trans
    implicit none
    real(8), allocatable :: a(:,:), e(:,:), p(:,:), q(:,:), r(:,:)
    real(8), allocatable :: ap(:,:), aq(:,:), ar(:,:)
    real(8), allocatable :: pa(:,:), qa(:,:), ra(:,:)
    real(8) c
    integer i, j, k, l, m

    write(*,*) 'input k (A_kl) : '
    read(*,*) k
    write(*,*) 'input l (A_kl) : '
    read(*,*) l
    write(*,*) 'input c : '
    read(*,*) c  
    write(*,*) 'input i : '
    read(*,*) i 
    write(*,*) 'input j : '
    read(*,*) j

    allocate(a(k,l), e(k,k), p(k,k), q(k,k), r(k,k))
    allocate(ap(k,k), aq(k,k), ar(k,k))
    allocate(pa(k,k), qa(k,k), ra(k,k))

    call random_seed
    call random_number(a(:,:))
    
    e(:,:) = func_id_mat(k)
    p(:,:) = func_p(c, e, i, k)
    q(:,:) = func_q(c, e, i, j, k)
    r(:,:) = func_r(e, i, j, k)
   
    write(*,*) 'matrix A :'
    do m = 1, k
        write(*,*) a(m,:)
    end do
    write(*,*) 'matrix P :'
    do m = 1, k
        write(*,*) p(m,:)
    end do
    write(*,*) 'matrix Q :'
    do m = 1, k
        write(*,*) q(m,:)
    end do
    write(*,*) 'matrix R :'
    do m = 1, k
        write(*,*) r(m,:)
    end do

    pa(:,:) = matmul(p, a)
    qa(:,:) = matmul(q, a)
    ra(:,:) = matmul(r, a)

    write(*,*) 'A*P ='
    do m = 1, k
        write(*,*) pa(m,:)
    end do
    write(*,*) 'A*Q ='
    do m = 1, k
        write(*,*) qa(m,:)
    end do
    write(*,*) 'A*R ='
    do m = 1, k
        write(*,*) ra(m,:)
    end do

    ap(:,:) = matmul(a, p)
    aq(:,:) = matmul(a, q)
    ar(:,:) = matmul(a, r)

    write(*,*) 'P*A ='
    do m = 1, k
        write(*,*) ap(m,:)
    end do
    write(*,*) 'Q*A ='
    do m = 1, k
        write(*,*) aq(m,:)
    end do
    write(*,*) 'R*A ='
    do m = 1, k
        write(*,*) ar(m,:)
    end do

end program elemenatary_transformation