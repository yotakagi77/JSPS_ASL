module globals
    real(kind(1d0)),allocatable :: a(:,:)
    real(kind(1d0)) sum
    integer n,count
end module globals

subroutine set_dd_mat
    use globals
    implicit NONE
    integer i,j
    do i=1,n
        do j=1,n
            if(i==j) then
                a(i,j)=1.0d0
            else
                call random_seed
                call random_number(a(i,j))
            end if
        end do
    end do
end subroutine set_dd_mat

subroutine judge
    use globals
    implicit NONE
    integer i,j
    do i=1,n
        sum=0.0d0
        do j=1,n
            if(i/=j) then
                sum=sum+abs(a(i,j))
            end if
        end do
        if(a(i,i)<=sum) then
            !write(*,*) sum
            count=0
            exit
        else 
            !write(*,*) sum
            count=1
        end if 
    end do
end subroutine judge

program name
    use globals
    implicit none
    integer i
    write(*,"(a)",advance="no") "input n :"
    read(*,*) n
    allocate(a(n,n))

    do 
        call set_dd_mat
        call judge
        if (count==1) then
            exit
        end if
    end do

    write(*,*) "A="
    do i=1,n
        write(*,"(100e12.4)") a(i,1:n)
    end do
end program name