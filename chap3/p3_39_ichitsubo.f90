module subprog
    implicit NONE
contains
    function normal_vec2(v,n) result(nv)
        integer,intent(in):: n
        real(kind(1d0)),intent(in)::v(n)
        real(kind(1d0)) nv(n),vl
        vl=sqrt(dot_product(v,v))
        if(vl==0.0d0) then
            nv(:)=0.0d0
        else 
            nv(:)=v(:)/vl
        end if
    end function normal_vec2

    function gs(a,n) result(e)
        integer,intent(in)::n
        real(kind(1d0)),intent(in)::a(n,n)
        real(kind(1d0)) e(n,n),dot_p
        integer k,j
        e(1:n,1)=normal_vec2(a(1:n,1:1),n)
        do k=2,n
            e(1:n,k)=a(1:n,k)
            do j=1,k-1
                dot_p=dot_product(a(1:n,k),e(1:n,j))
                e(1:n,k)=e(1:n,k)-dot_p*e(1:n,j)
            end do
            e(1:n,k)=normal_vec2(e(1:n,k:k),n)
        end do
    end function gs

    function adj(a) result(a_adj)
        integer i,j
        real(kind(1d0)),intent(in) :: a(3,3)
        real(kind(1d0)) a_adj(3,3),dummy(3,3)
        do i=1,3
            do j=1,3
                dummy=cshift(a,i,1)
                dummy=cshift(dummy,j,2)
                a_adj(i,j)=(dummy(1,1)*dummy(2,2)-dummy(1,2)*dummy(2,1))
            end do
        end do

    end function adj

    function det2(a) result(a_det)
        integer j
        real(kind(1d0)),intent(in) :: a(3,3)
        real(kind(1d0)) a_det,a_adj(3,3)
        a_adj=adj(a)
        a_det=0.0d0
        do j=1,3
                a_det=a_det+a(1,j)*a_adj(1,j)
        end do
    end function det2

end module subprog

program name
    use subprog
    implicit none
    integer i
    real(kind(1d0)) a(3,3),b(3,3),e(3,3),c(3,3)

    call random_seed
    call random_number(a)
    call random_number(b)

    e=gs(b,3)
    write(*,*) "T="
    do i=1,3
        write(*,*) e(i,:)
    end do

    write(*,*) "|A|="
    write(*,*) det2(a)
    write(*,*) "|T^TAT|="
    c=matmul(transpose(e),a)
    c=matmul(c,e)
    write(*,*) det2(c)


    
end program name