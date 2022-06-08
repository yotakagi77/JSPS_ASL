program name
    implicit none
    integer n,i,j,k
    real(kind(1d0)) ,allocatable :: a(:,:),b(:,:),c(:,:),c0(:,:),tmpa(:,:),tmpb(:,:)
    write(*,*) "input n :"
    read(*,*) n
    allocate(a(n,n),b(n,n),c(n,n),c0(n,n),tmpa(n,n),tmpb(n,n))

    call random_seed
    call random_number(a)
    call random_number(b)
    write(*,*) "a="
    do i=1,n
    write(*,*) a(i,:)
    end do

    write(*,*) "b="
    do i=1,n
    write(*,*) b(i,:)
    end do

    do j=1,n
        do i=1,n
            c(i,j)=0.0d0
            do k=1,n
                c(i,j)=c(i,j)+a(i,k)*b(k,j)
            end do
        end do
    end do

    c0=c
    

    do i=1,n
        do j=1,n
            c(i,j)=c0(j,i)
        end do
    end do

    write(*,*) "(AB)^T(without_kumikomi)="
    do i=1,n
    write(*,*) c(i,:)
    end do

    c=transpose(matmul(a,b))
    write(*,*) "(AB)^T(use_kumikomi)="
    do i=1,n
    write(*,*) c(i,:)
    end do



    tmpa=a

    do i=1,n
        do j=1,n
            a(i,j)=tmpa(j,i)
        end do
    end do

    tmpb=b

    do i=1,n
        do j=1,n
            b(i,j)=tmpb(j,i)
        end do
    end do

    do j=1,n
        do i=1,n
            c(i,j)=0.0d0
            do k=1,n
                c(i,j)=c(i,j)+b(i,k)*a(k,j)
            end do
        end do
    end do

    write(*,*) "A^T*B^T(without_kumikomi)="
    do i=1,n
    write(*,*) c(i,:)
    end do

    c=matmul(transpose(tmpb),transpose(tmpa))
    write(*,*) "A^T*B^T(use_kumikomi)="
    do i=1,n
    write(*,*) c(i,:)
    end do




end program name
