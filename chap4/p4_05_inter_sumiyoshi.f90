module interface_mod
 interface
  function chk_dd_mat(n,a) result(count)
    real(8),allocatable :: a(:,:)
    real(8) sum
    integer n,count
  end function chk_dd_mat 
 end interface
end module interface_mod