program name
    implicit none
    integer n,i
    real(kind(1d0)),allocatable :: x(:),y(:),r(:)
write(*,*) "input n :"
read(*,*) n

allocate(x(n),y(n),r(n))
x(1)=0
do i=2,n
    x(i)=x(i-1)+10/dble(n-1)
end do
call random_seed
call random_number(r)
r(:)=2.0d0*r(:)-1.0d0

y(:)=2.0d0*x(:)+1.0d0+r(:)

open(17,file="output2_12.d") 
do i=1,n
write(17,*) x(i),y(i)
end do
    
end program name