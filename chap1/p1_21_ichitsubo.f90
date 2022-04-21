program name
    implicit none
    integer n
    real(kind(1d0)) r,e
    write(*,*) "input r:"
    read(*,*) r
    
    open(17,file='output1_21.d', status='replace')
    do n=3,100
        e=ABS(acos(-1d0)*r*r-r*r*sin(2*acos(-1d0)/dble(n))*dble(n)/2.0d0)
        write(17,*) n,e
    end do
end program name