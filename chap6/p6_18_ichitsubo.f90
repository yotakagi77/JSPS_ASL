subroutine set_bdc(phi,x1,n1,n2)
    implicit NONE
    real(kind(1d0)) phi(n1,n2),x1(n1)
    integer n1,n2,i,j

    do i=1,n1
        phi(i,1)=sin(acos(-1.0d0)*x1(i))
    end do

    do i=1,n1
        phi(i,n2)=0.0d0
    end do

    do j=1,n2
        phi(1,j)=0.0d0
    end do

    do j=1,n2
        phi(n1,j)=0.0d0
    end do


end subroutine set_bdc

subroutine chk_err(phi,c,d,n1,n2,er)
    implicit none
    integer n1,n2,i,j
    real(kind(1d0)) phi(n1,n2),c,d,er,rhs
    er=0.0d0
    do i=2,n1-1
        do j=2,n2-1
            rhs=-c*(phi(i-1,j)+phi(i+1,j))  &
                    -d*(phi(i,j-1)+phi(i,j+1))
            er=er+(rhs-phi(i,j))**2.0d0
        end do
    end do
end subroutine chk_err

program name
    implicit none
    real(kind(1d0)) rhs,c,d,omg,dx1,dx2,er
    real(kind(1d0)),allocatable::phi(:,:),x1(:),x2(:)
    integer i,j,itr,itrmax,n1,n2

    n1=101
    n2=101
    allocate(phi(n1,n2),x1(n1),x2(n2))
    dx1=1.0d0/dble(n1-1)
    dx2=1.0d0/dble(n2-1)
    c=-dx2**2.0d0/(dx1**2.0d0+dx2**2.0d0)/2.0d0
    d=c*(dx1/dx2)**2.0d0

    do i=1,n1
            x1(i)=0.0d0+dx1*dble(i-1)
    end do

    do j=1,n2
            x2(j)=0.0d0+dx2*dble(j-1)
    end do

    call set_bdc(phi,x1,n1,n2)
    itrmax=10**5.0d0

    omg=2.0d0/(1+sin(acos(-1.0d0)/dble(n1-1)))

    do itr=1,itrmax
        do j=2,n2-1
            do i=2,n1-1
                rhs=-c*(phi(i-1,j)+phi(i+1,j))  &
                    -d*(phi(i,j-1)+phi(i,j+1))
                phi(i,j)=phi(i,j)+omg*(rhs-phi(i,j))
            end do
        end do
        call chk_err(phi,c,d,n1,n2,er)

        write(*,*) "itr,er=",itr,er
        if(er<10**(-6.0d0)) exit
    end do

    open(17,file="SOR_x1_0.5.d")
    open(27,file="SOR_x2_0.5.d")
    do j=1,n2
        do i=1,n1
            if (x1(i)==0.5d0) then
            write(17,"(3e12.4)") x2(j),phi(i,j)
            end if
            if (x2(j)==0.5d0) then
                write(27,"(3e12.4)") x1(i),phi(i,j)
            end if
        end do
    end do

    close(17)
    close(27)
    open(17,file="theory_x1_0.5.d")
    open(27,file="theory_x2_0.5.d")
    do j=1,n2
        do i=1,n1
            if (x1(i)==0.5d0) then
                write(17,"(3e12.4)") x2(j),sin(acos(-1.0d0)*x1(i))*sinh(acos(-1.0d0)*(1-x2(j)))/sinh(acos(-1.0d0))
            end if
            if (x2(j)==0.5d0) then
                write(27,"(3e12.4)") x1(i),sin(acos(-1.0d0)*x1(i))*sinh(acos(-1.0d0)*(1-x2(j)))/sinh(acos(-1.0d0))
            end if
        end do
    end do



end program name