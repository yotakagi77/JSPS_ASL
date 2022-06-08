module mat_subprogs
    implicit NONE
contains
    subroutine print_rmatc(a,title)
        real(kind(1d0)) ,intent(in)::a(:,:)
        integer i,n,m
        character(*),intent(in)::title
        n=size(a,1)
        m=size(a,2)
        write(*,*) title
        do i=1,n
            write(*,"(100e12.4)") a(i,1:m)
        end do
    end subroutine print_rmatc
    subroutine print_imatc(a,title)
        integer ,intent(in)::a(:,:)
        integer i,n,m
        character(*),intent(in)::title
        n=size(a,1)
        m=size(a,2)
        write(*,*) title
        do i=1,n
            write(*,"(100i12.2)") a(i,1:m)
        end do
    end subroutine print_imatc
end module mat_subprogs

program name
    use mat_subprogs
    implicit none
    integer im,jm
    real(kind(1d0)),allocatable :: a(:,:)
    integer,allocatable :: ia(:,:)
    write(*,*) "input im"
    read(*,*) im
    write(*,*) "input jm"
    read(*,*) jm
    allocate(a(im,jm),ia(im,jm))
    call random_seed
    call random_number(a)
    ia=int(a*1000)

    call print_rmatc(a,"matrix a")
    call print_imatc(ia,"matrix ia")
    
end program name