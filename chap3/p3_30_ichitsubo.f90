module subprog
    implicit NONE
contains
    function trans(a,n) result(aT)
        integer,intent(in) ::n
        integer i,j
        real(kind(1d0)) ,intent(in) :: a(n,n)
        real(kind(1d0)) aT(n,n)
        do i=1,n
            do j=1,n
                aT(i,j)=a(j,i)
            end do
        end do

    end function trans
end module subprog

program  name
    use subprog
    implicit none
    integer n,i
    real(kind(1d0)),allocatable :: a(:,:),x(:,:)
    write(*,*) "input n:"
    read(*,*) n
    allocate(a(n,n),x(n,n))
    call random_seed
    call random_number(a)

    write(*,*) "a="
    do i=1,n
        write(*,*) a(i,:)
    end do

    write(*,*) "aT(module)="
    x=trans(a,n)
    do i=1,n
        write(*,*) x(i,:)
    end do

    write(*,*) "aT(kumikomi)="
    x=transpose(a)
    do i=1,n
        write(*,*) x(i,:)
    end do

    
end program name