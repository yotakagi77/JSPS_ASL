module mat_subprogs
    implicit NONE
contains
    recursive function det_mat(a,n) result(det)
    integer,intent(in) :: n
    real(kind(1d0)),intent(in) :: a(n,n)
    real(kind(1d0)) det,b(n-1,n-1)
    integer i
    if (n>1) then
        det = 0.0d0
        do i=1,n
            b(1:i-1,1:n-1)=a(1:i-1,2:n)
            b(i:n-1,1:n-1)=a(i+1:n,2:n)
            det=det+(-1.0d0)**dble(i+1)&
                *a(i,1)*det_mat(b,n-1)
        end do
    else
        det=a(1,1)
    end if
    end function det_mat
end module mat_subprogs

program name
    use mat_subprogs
    implicit none
    integer i
    integer,parameter :: n=5
    real(kind(1d0)) a(n,n)
    call random_seed
    call random_number(a)
    
    write(*,*) "A="
    do i=1,n
        write(*,"(100e12.4)") a(i,1:n)
    end do
    write(*,*) "det=",det_mat(a,n)

end program name