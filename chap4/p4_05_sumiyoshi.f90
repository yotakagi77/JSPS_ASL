module globals
    real(8),allocatable :: a(:,:)
    real(8) sum
    integer n,count
end module globals

subroutine chk_dd_mat
    use globals
    implicit none
    integer i,j
    do i=1,n
        sum=0.0d0
        do j=1,n
            if(i/=j) then
                sum=sum+abs(a(i,j))
            end if
        end do
        if(a(i,i)<=sum) then
            count=0
            exit
        else
            count=1
        end if 
    end do
end subroutine chk_dd_mat

program judge
    use globals
    implicit none
    integer i
    write(*,*) "input n :"
    read(*,*) n
    allocate(a(n,n))
    read(*,*) a
     call chk_dd_mat
     write(*,*) count
end program judge