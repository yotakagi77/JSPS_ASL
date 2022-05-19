program name
    implicit none
    integer n,n0,i,j,k,count
    real(kind(1d0)) t1,t2
    real(kind(1d0)) ,allocatable :: a(:,:),b(:,:),c(:,:),c2(:),b2(:)
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
    write(17,*) n,t2-t1
    write(*,*) "n(MAT*MAT)=",n,"time(MAT*MAT)=",t2-t1
    n=n+n0
end do
n=n0*10*2
do count=1,10
    allocate(a(n,n),b2(n),c2(n))
    call random_seed
    call random_number(a)
    call random_number(b2)
    call cpu_time(t1)
    do i=1,n
        c2(i)=0.0d0
        do j=1,n
                c2(i)=c2(i)+a(i,j)*b2(j)
        end do
    end do
    call cpu_time(t2)
    deallocate(a,b2,c2)
    write(17,*) n,t2-t1
    write(*,*) "n(MAT*vec)=",n,"time(MAT*vec)=",t2-t1
    n=(count+1)*n0*10*2
end do

end program name
