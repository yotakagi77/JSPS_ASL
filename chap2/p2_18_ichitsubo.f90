program name
    implicit none
    integer n,n0,i,j,k,count
    real(kind(1d0)) t1,t2
    real(kind(1d0)) ,allocatable :: a(:,:),b(:,:),c(:,:)
    write(*,*) "input n :"
    read(*,*) n0
    n=n0
    !allocate(a(n,n),b(n,n),c(n,n))
    open(17,file="output2_18.dat")
do count=1,10
    allocate(a(n,n),b(n,n),c(n,n))
    call random_seed
    call random_number(a)
    call random_number(b)
    call cpu_time(t1)
    do j=1,n
        do i=1,n
            c(i,j)=0.0d0
            do k=1,n
                c(i,j)=c(i,j)+a(i,k)*b(k,j)
            end do
        end do
    end do
    call cpu_time(t2)
    deallocate(a,b,c)
    n=n+n0
    write(17,*) n-n0,t2-t1
    write(*,*) "n=",n-n0,"time=",t2-t1
end do

end program name