subroutine gauss_jordan_pv(a0,x,b,n)
    ! --- ガウス-ジョルダン(部分pivot選択有) ---
    integer,intent(in) :: n
    real(kind(1d0)),intent(in) :: a0(n,n),b(n)
    real(kind(1d0)),intent(out) :: x(n)
    integer i,k,m
    real(kind(1d0)) ar,am,t,a(n,n),w(n)

    a(:,:) = a0(:,:)
    x(:) = b(:)

    do k=1,n
        ! ---部分pivot選択
        m=k
        am=abs(a(k,k))
        do i=k+1,n
            if(abs(a(i,k))>am) then
                am=abs(a(i,k))
                m=i
            end if
        end do

        if (am==0.0d0) stop "A is singular"

        if(k/=m) then
            w(k:n)=a(k,k:n)
            a(k,k:n)=a(m,k:n)
            a(m,k:n)=w(k:n)
            t=x(k)
            x(k)=x(m)
            x(m)=t
        end if

        ar=1.0d0/a(k,k)
        a(k,k)      =1.0d0
        a(k,k+1:n)=ar*a(k,k+1:n)
        x(k)=ar*x(k)

        do i=1,n
            if (i/=k) then
                a(i,k+1:n) = a(i,k+1:n)-a(i,k)*a(k,k+1:n)
                x(i)=x(i)-a(i,k)*x(k)
                a(i,k)=0.0d0
            end if
        end do

    end do
end subroutine gauss_jordan_pv

program name
    implicit none
    integer n,i
    real(kind(1d0)) ,allocatable :: a0(:,:),b(:),x(:)
    
    write(*,*) "input n :"
    read(*,*) n

    allocate(a0(n,n),b(n),x(n))

    a0(1,1)=1
    a0(1,2)=-2
    a0(1,3)=3
    a0(2,1)=2
    a0(2,2)=-1
    a0(2,3)=1
    a0(3,1)=1
    a0(3,2)=3
    a0(3,3)=-5

    b(1)=5
    b(2)=6
    b(3)=2

    call random_seed
    call random_number(a0)
    call random_number(b)

    call gauss_jordan_pv(a0,x,b,n)

    write(*,*) "a0="
    do i=1,n
        write(*,*) a0(i,:)
    end do

    write(*,*) "b="
    do i=1,n
        write(*,*) b(i)
    end do

    write(*,*) "x="
    do i=1,n
        write(*,*) x(i)
    end do


end program name