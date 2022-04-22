module subprog
    implicit none
contains
    subroutine inverse(a)
        integer i
        real(kind(1d0)),intent(in)::a(2,2)
        real(kind(1d0)) a_1(2,2),a_det
        a_det=a(1,1)*a(2,2)-a(1,2)*a(2,1)
        if (a_det==0.0d0) stop "stop (|A|=0)"
        a_1(1,1)=a(2,2)/a_det
        a_1(1,2)=-a(1,2)/a_det
        a_1(2,1)=-a(2,1)/a_det
        a_1(2,2)=a(1,1)/a_det
        write(*,*)"A="
        do i=1,2
        write(*,*) a(i,:)
        end do
        write(*,*)"|A|="
        write(*,*) a_det
        write(*,*)"A^-1="
        do i=1,2
        write(*,*) a_1(i,:)
        end do

    end subroutine inverse
end module subprog

program name
    use subprog
    implicit none
    real(kind(1d0)) a(2,2)

    call random_seed
    call random_number(a)
    call inverse(a)
    
end program name