module interface_mod
  interface
    subroutine rmat(a,n)
	  double precision,intent(out) ::a(n,n)
	end subroutine rmat

	subroutine print_mat(a)
	  double precision,intent(in) :: a(:,:)
	end subroutine print_mat
  end interface
end module interface_mod