module interface_mod
 interface
  function rechar(c) result(rc)
   character(*), intent(in) :: c
   character(len(c))  rc
  end function rechar
 end interface
end module interface_mod