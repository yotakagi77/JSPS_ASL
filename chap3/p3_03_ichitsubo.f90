module subprog
    implicit NONE
contains

subroutine swap(a,b)
    integer a,b
    integer tmp
    tmp=a
    a=b
    b=tmp

    write(*,*) "tmp=",tmp
end subroutine swap

end module subprog

program name
    use subprog
    implicit none
    integer :: x=77,y=9095,tmp=0
    !write(*,*) "x,y,tmp=",x,y,tmp
    write(*,*) "tmp=",tmp
    call swap(tmp,y)
    write(*,*) "x,y,tmp=",x,y,tmp
    !write(*,*) tmp


end program name