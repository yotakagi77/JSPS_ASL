module subprog
    implicit none
contains
    function det1(a) result(a_det)
        real(kind(1d0)),intent(in) :: a(2,2)
        real(kind(1d0)) a_det
        a_det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
    end function det1

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
    integer n
    real(kind(1d0)) a1(3),a2(3),a3(3),c(2,3),dummy(2,3),x(3),sca_tri,M(3,3)
    
    call random_seed
    call random_number(a1)
    call random_number(a2)
    call random_number(a3)

    c(1,:)=a2(:)
    c(2,:)=a3(:)

    do n=1,3
        dummy=cshift(c,n,2)
        x(n)=det1(dummy)
    end do

    sca_tri=dot_product(x,a1)
    write(*,*) "a(b*c)="
    write(*,*) sca_tri

    M(:,1)=a1
    M(:,2)=a2
    M(:,3)=a3

    write(*,*) "|M|="
    write(*,*) det2(M)
    


end program name