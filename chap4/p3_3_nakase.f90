
program chk
 use interface_mod
 implicit none
 character(11) :: c = 'I prefer Pi'
 write(*,*) c
 write(*,*) rechar(c)
end program chk