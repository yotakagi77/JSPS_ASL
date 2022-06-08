subroutine func(x,y) 
    implicit NONE
    real(kind(1d0)) x,y,r
    y=0.1*x**3+0.2*x**2+0.5*x+1
    call random_seed
    call random_number(r)
    y=y+2.0d0*r-1.0d0
    return
end subroutine func

program name
    implicit none
    real(kind(1d0)) ,allocatable:: x(:),y(:)
    real(kind(1d0)) a
    integer m,i
    write(*,*) "input m :"
    read(*,*) m
    allocate(x(m),y(m))
    open(17,file="output6_09.d")
    x(1)=-10.0d0
    do i=2,m
        x(i)=x(i-1)+20.0d0/dble(m)
    end do

    do i=1,m
        call func(x(i),a)
        write(17,*) x(i),a
    end do


end program name