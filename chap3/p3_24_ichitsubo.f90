module subprog
    implicit none
contains
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

    function det(a) result(a_det)
        integer j
        real(kind(1d0)),intent(in) :: a(3,3)
        real(kind(1d0)) a_det,a_adj(3,3)
        a_adj=adj(a)
        a_det=0.0d0
        do j=1,3
                a_det=a_det+a(1,j)*a_adj(1,j)
        end do
    end function
end module subprog

program name
    use subprog
    implicit none
    integer i,j
    real(kind(1d0)) a(3,3),aT(3,3),a_up_tri(3,3)
    
    call random_seed
    call random_number(a)

    do i=1,3
        do j=1,3
            aT(i,j)=a(j,i)
        end do
    end do

    write(*,*) "A="

    do i=1,3
    write(*,*) a(i,:)
    end do

    write(*,*) "|A^T|="
    write(*,*) det(aT)
    write(*,*) "|A|="
    write(*,*) det(a)

    a_up_tri=a

    do i=1,3
        do j=1,3
            if (i>j) then
                a_up_tri(i,j)=0.0d0
            end if
        end do
    end do

    write(*,*) "A_up_tri="

    do i=1,3
    write(*,*) a_up_tri(i,:)
    end do

    write(*,*) "|A_up_tir|="
    write(*,*) det(a_up_tri)
    write(*,*) "a11*a22*a33="
    write(*,*) a_up_tri(1,1)*a_up_tri(2,2)*a_up_tri(3,3)

end program name