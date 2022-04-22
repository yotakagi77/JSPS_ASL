module subprog
    implicit none
contains
    function det(a) result(a_det)
        real(kind(1d0)),intent(in) :: a(2,2)
        real(kind(1d0)) a_det
        a_det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
    end function det
end module subprog

program name
    use subprog
    implicit none
    integer n,i
    real(kind(1d0)) a(3),b(3),c(2,3),dummy(2,3),x(3)
    
    call random_seed
    call random_number(a)
    call random_number(b)

    c(1,:)=a(:)
    c(2,:)=b(:)

    write(*,*) "c="
    do i=1,2
    write(*,*) c(i,:)
    end do

    do n=1,3
        dummy=cshift(c,n,2)
        x(n)=det(dummy)
    end do

    write(*,*) "i1:"
    write(*,*) x(1)
    write(*,*) "i2:"
    write(*,*) x(2)
    write(*,*) "i3:"
    write(*,*) x(3)


end program name