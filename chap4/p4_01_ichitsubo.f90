module interface_mod
    interface
        subroutine allocate_rmat(a,im,jm)
            real(kind(1d0)),allocatable,intent(out)::a(:,:)
            integer im,jm
        end subroutine allocate_rmat

        subroutine print_mat(a,im,jm)
            real(kind(1d0)),intent(in) :: a(im,jm)
            integer i,im,jm
        end subroutine print_mat

    end interface
end module interface_mod

subroutine allocate_rmat(a,im,jm)
    implicit none
    real(kind(1d0)),allocatable,intent(out) :: a(:,:)
    integer n,im,jm
    write(*,"(a)",advance="no") "input n :"
    read(*,*) n
    im=n
    jm=n
    if (n<1 .or. n>100) stop "n must be 0 < n < 101"
    allocate(a(im,jm))
    call random_number(a)
end subroutine allocate_rmat

subroutine print_mat(a,im,jm)
    implicit NONE
    real(kind(1d0)),intent(in) :: a(im,jm)
    integer i,im,jm
    do i=1,im
        write(*,"(100e12.4)") a(i,1:jm)
    end do
end subroutine print_mat

program name
    use interface_mod
    implicit none
    integer im,jm
    real(kind(1d0)),allocatable:: a(:,:)
    call allocate_rmat(a,im,jm)
    call print_mat(a,im,jm)
end program name