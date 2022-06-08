subroutine print_title(title)
   implicit none
   character(*), intent(in) :: title
   write(*,*) title, len(title)
end subroutine print_title