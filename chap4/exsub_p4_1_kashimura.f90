subroutine rmat(a,n)
  implicit none !各プログラムごとにこの宣言が必要
    double precision,intent(out) :: a(n,n) !仮配列は未割付けの割付け配列
    integer,intent(in) :: n
    call random_number(a)
end subroutine rmat

subroutine print_mat(a)
  implicit none
    double precision,intent(in) :: a(:,:) !形状引継ぎ配列
	integer :: i,n,m
	n=size(a,1)
	m=size(a,2)
	do i=1,n
	   write(*,'(100e12.4)') a(i,1:m)
	end do
end subroutine print_mat
	