!p100 演習3.16
module mat_subprogs      
  implicit none
contains

  subroutine allocate_rmat(a)
	double precision,allocatable,intent(out) :: a(:,:)
	integer :: n
	write(*,'(a)',advance='no') 'input n : '
	read(*,*) n
	if(n<1 .or. n>100) stop 'n must be 0<n<101'
	allocate(a(n,n))
	call random_number(a)
  end subroutine allocate_rmat
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
!転置行列を出力するサブルーチン 
  subroutine cal_at(a,at)
    double precision,intent(in) :: a(:,:)
    double precision,allocatable,intent(out) :: at(:,:)
	integer :: n
    n=size(a,1)
	allocate(at(n,n))
    at=transpose(a) !転置行列を得る組み込み関数
  end subroutine cal_at
!対称行列を出力するサブルーチン 
  subroutine cal_a1(a,a1)
    double precision,intent(in) :: a(:,:)
    double precision,allocatable,intent(out) :: a1(:,:)
	double precision,allocatable :: at(:,:)
	integer :: n
    n=size(a,1)
	allocate(a1(n,n),at(n,n))
    at=transpose(a)
    a1=(a+at)/2
  end subroutine cal_a1
!交代行列を出力するサブルーチン
  subroutine cal_a2(a,a2)
    double precision,intent(in) :: a(:,:)
    double precision,allocatable,intent(out) :: a2(:,:)
	double precision,allocatable :: at(:,:)
	integer :: n
    n=size(a,1)
	allocate(a2(n,n),at(n,n))
    at=transpose(a)
    a2=(a-at)/2
  end subroutine cal_a2
  
end module mat_subprogs

program random_mat
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:),at(:,:),a1(:,:),a2(:,:)
	write(*,*) 'matrix A(n,n)'
    call allocate_rmat(a)
	call print_mat2(a)
	write(*,*) 'matrix AT(n,n)'
    call cal_at(a,at)
	call print_mat2(at)
	write(*,*) 'matrix A1(n,n)'
	call cal_a1(a,a1)
	call print_mat2(a1)
	write(*,*) 'matrix A2(n,n)'
	call cal_a2(a,a2)
	call print_mat2(a2)
end program random_mat