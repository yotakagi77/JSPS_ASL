module subprog
implicit none
contains

subroutine out
open(10, file = 'output.d')
end subroutine out

subroutine count
integer, save ::  ic = 0

if(ic == 10) then
stop
else
ic = ic +1
write(10 ,*) ic
endif

end subroutine count

end module subprog

program main
use subprog
implicit none
integer i
 call out
do i = 1, 10
 call count
end do
end program main