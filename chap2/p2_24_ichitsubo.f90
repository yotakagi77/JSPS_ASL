program name
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
        x(n)=dummy(2,2)*dummy(1,1)-dummy(1,2)*dummy(2,1)
    end do

    write(*,*) "i1:"
    write(*,*) x(1)
    write(*,*) "i2:"
    write(*,*) x(2)
    write(*,*) "i3:"
    write(*,*) x(3)


end program name