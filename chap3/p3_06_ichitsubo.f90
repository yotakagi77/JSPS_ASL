module subprog
    implicit NONE
contains

subroutine sum(i,j,k)
    integer ,intent(in) :: i,j
!intent属性がinである変数への代入
    integer,intent(in) :: k
    k=i+j

end subroutine sum

end module subprog

program name
    use subprog
    implicit none
    integer :: i=3,j=5,k=0
    call sum(i,j,k)
    write(*,*) "i,j,k=",i,j,k


end program name