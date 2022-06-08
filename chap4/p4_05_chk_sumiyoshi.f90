function chk_dd_mat(n,a) result(count)
 implicit none
  real(8),allocatable :: a(:,:)
  real(8) sum
  integer n,count
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
end function chk_dd_mat