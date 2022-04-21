program name
    implicit none
    real(kind(1d0)) k,a,xk
    write(*,*) "input k:" 
    read(*,*) k
    if (k<2) stop "stop"
    write(*,*) "input a:" 
    read(*,*) a
    if (a<0) stop "stop"
    xk=1.0d0
    do 
        xk=xk-(xk**k-a)/(k*xk**(k-1))
        if (abs(xk**k-a)<10.0d0**(-10)) then
        exit
        end if
        write(*,*) xk
    end do

    write(*,*) xk

end program name