subroutine set_dd_mat(a,n)
    implicit NONE
    real(kind(1d0)) a(n,n)
    integer i,j,n
    do i=1,n
        do j=1,n
            if(i==j) then
               a(i,j)=dble(n)
            else
                call random_seed
                call random_number(a(i,j))
            end if
        end do
    end do
end subroutine set_dd_mat

subroutine judge(a,n,count)
    implicit NONE
    integer i,j,count,n
    real(kind(1d0)) sum,a(n,n)
    do i=1,n
        sum=0.0d0
        do j=1,n
            if(i/=j) then
                sum=sum+abs(a(i,j))
            end if
        end do
        if(a(i,i)<=sum) then
            write(*,*) sum
            count=0
            exit
        else 
            write(*,*) sum
            count=1
        end if 
    end do
end subroutine judge

program name
    implicit none
    real(kind(1d0)) ,allocatable :: a(:,:),xk(:),xk_1(:),b(:),temp(:)
    real(kind(1d0)) sigma,zansa
    integer n,i,j,itr_Jacobi,itr_GS,count
    write(*,*) "input n :"
    read(*,*) n
    allocate(a(n,n),xk(n),xk_1(n),b(n),temp(n))

    call random_seed
    call random_number(b)

    sigma=0.0d0
    itr_Jacobi=0
    do 
        call set_dd_mat(a,n)
        call judge(a,n,count)
        if (count==1) then
            exit
        end if
    end do





    do 
        itr_Jacobi=itr_Jacobi+1
        xk_1=xk

        do i=1,n
            sigma=0.0d0
            do j=1,i-1
                sigma=sigma+a(i,j)*xk_1(j)
            end do
            do j=i+1,n
                sigma=sigma+a(i,j)*xk_1(j)
            end do
            if (a(i,i)==0) stop "a(i,i)==0.0d0"
            xk(i)=(b(i)-sigma)/a(i,i)

        end do

        temp=matmul(a,xk)
        zansa=sqrt(dot_product(b-temp,b-temp))
        write(*,*) "itr_Jacobi=",itr_Jacobi,"err=",zansa
        if (zansa<10**(-6.0d0)) then
            exit
        end if

        !if(itr==30) then
        !exit
        !end if
    end do
    xk_1=0.0d0
    xk=0.0d0
    itr_GS=0
    do 
        itr_GS=itr_GS+1
        xk_1=xk

        do i=1,n
            sigma=0.0d0
            do j=1,i-1
                sigma=sigma+a(i,j)*xk(j)
            end do
            do j=i+1,n
                sigma=sigma+a(i,j)*xk_1(j)
            end do
            if (a(i,i)==0) stop "a(i,i)==0.0d0"
            xk(i)=(b(i)-sigma)/a(i,i)

        end do

        temp=matmul(a,xk)
        zansa=sqrt(dot_product(b-temp,b-temp))
        write(*,*) "itr_GS=",itr_GS,"err=",zansa
        if (zansa<10**(-6.0d0)) then
            exit
        end if

        !if(itr==30) then
        !exit
        !end if
    end do


    write(*,*) "a="
    do i=1,n
        write(*,*) a(i,:)
    end do

    write(*,*) "b="
    do i=1,n
        write(*,*) b(i)
    end do

    write(*,*) "xk="
    do i=1,n
        write(*,*) xk(i)
    end do


end program name