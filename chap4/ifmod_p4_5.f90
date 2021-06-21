module interface_mod
 interface
  function chk_dd_mat(a,n) result(x)
   integer, intent(in)::n
   real(8), intent(in)::a(1:n,1:n)
   integer x
  end function chk_dd_mat
 end interface
end module interface_mod