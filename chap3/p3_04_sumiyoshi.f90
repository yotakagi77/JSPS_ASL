module subprog
implicit none
contains

subroutine count
integer, save ::  ic = 1

if(ic == 1) then
 open(10, file = 'output.d')
 write(10 ,*) ic
endif
if(ic == 10) then
 close(10)
stop
else
 ic = ic + 1
write(10 ,*) ic
endif

end subroutine count

end module subprog

program main
use subprog
implicit none
integer i
do i = 1, 10
 call count
end do
end program main