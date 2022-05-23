module interface_mod
 interface
  subroutine print_title(title)
   character(*), intent(in) :: title
  end subroutine print_title
 end interface
end module interface_mod

subroutine print_title(title)
   implicit none
   character(*), intent(in) :: title
   write(*,*) title, len(title)
end subroutine print_title

program moji
 use interface_mod
 implicit none
 character(5) :: c = 'hello'
 call print_title(c)
 call print_title('good bye')
end program moji