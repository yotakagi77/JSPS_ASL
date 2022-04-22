module subprog
    implicit NONE
contains
    function sekibun(a,b) result(s)
        real(kind(1d0)) ,intent(in)::a,b
        real(kind(1d0)) s,x,dx
        x=a
        s=0.0d0
        dx=10**(-6.0d0)
        do
            if (x>b) then
                exit
            end if
            s=s+(exp(-x*x/2.0d0)/sqrt(2*acos(-1.0d0))+exp(-(x+dx)*(x+dx)/2.0d0)/sqrt(2*acos(-1.0d0)))*dx/2
            x=x+dx
        end do

    end function sekibun
end module subprog

program main
    use subprog
    implicit NONE
    real(kind(1d0)) a,b

    write(*,*) "input a:"
    read(*,*) a
    write(*,*) "input b:"
    read(*,*) b

    write(*,*) "value=",sekibun(a,b)

end program main