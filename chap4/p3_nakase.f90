module interface_mod
 interface
  function rechar(c) result(rc)
   character(*), intent(in) :: c
   character(len(c))  rc
  end function rechar
 end interface
end module interface_mod

function rechar(c) result(rc)
implicit none
character(*), intent(in) :: c
character(len(c)) rc
integer i
do i = 1, len(c)
 rc(i : i) = c(len(c) - i + 1 : len(c) - i + 1)
end do
end function rechar

program chk
 use interface_mod
 implicit none
 character(11) :: c = 'I prefer Pi'
 write(*,*) c
 write(*,*) rechar(c)
end program chk