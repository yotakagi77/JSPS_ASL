program name
    implicit none
    integer n1,n2,i,j
    real(kind(1d0)),allocatable :: x(:,:,:),phi(:,:)
    write(*,*) "input n_1 :"
    read(*,*) n1
    write(*,*) "input n_2 :"
    read(*,*) n2
    allocate(x(2,n1,n2),phi(n1,n2))
    x(:,:,:)=0.0d0
        do i=1,n1
            do j=1,n2
            x(1,i,j)=1.0d0/dble(n1-1)*dble(i-1)
            x(2,i,j)=1.0d0/dble(n2-1)*dble(j-1)
            end do
        end do
        do i=1,n1
            do j=1,n2
                phi(i,j)=sin(acos(-1.0d0)*x(1,i,j))*sinh(acos(-1.0d0)*(1.0d0-x(2,i,j)))/sinh(acos(-1.0d0))
            end do
        end do
    open(17,file="output2_16.d")
    do i=1,n1
        do j=1,n2
    write(17,*) x(1,i,j),x(2,i,j),phi(i,j)
        end do
        write(17,*) ""
    end do

end program name