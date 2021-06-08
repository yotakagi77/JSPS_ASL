!p107 演習3.28
module mat_subprogs      
  implicit none
contains
!n×nの単位行列(identity matrix)を定義するサブルーチン
  subroutine ide_mat(a)
	double precision,allocatable,intent(out) :: a(:,:)
	integer :: i,j,n
	write(*,'(a)',advance='no') 'input n : '
	read(*,*) n
	if(n<1 .or. n>100) stop 'n must be 0<n<101'
	allocate(a(n,n))
	a=reshape([(1,(0,i = 1,n),j=1,n-1),1],[n, n])
  end subroutine ide_mat
!m×n行列を書き出すサブルーチン
  subroutine print_mat2(a)
    double precision,intent(in) :: a(:,:)
	integer :: i,n,m
	m=size(a,1) !組み込み関数sizeによりaの最初の次元の寸法を得る
	n=size(a,2) !組み込み関数sizeによりaの二番目の次元の寸法を得る
	do i=1,m
	   write(*,'(100e12.4)') a(1:n,i)
	end do
  end subroutine print_mat2
end module mat_subprogs



program main
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:)
	!行列Aの設定
	write(*,*) 'identity matrix A(n,n)'
    call ide_mat(a)
	call print_mat2(a)
end program main