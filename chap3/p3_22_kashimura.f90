!p101 演習22
module mat_subprogs      
  implicit none
contains
!n×n行列を乱数を用いて定義するサブルーチン
  subroutine allocate_rmat(a,n)
	double precision,allocatable,intent(out) :: a(:,:)
	integer :: i,n
	!write(*,'(a)',advance='no') 'input n : '
	!read(*,*) n
	!if(n<1 .or. n>100) stop 'n must be 0<n<101'
	allocate(a(n,n))
	call random_number(a)
	do i=1,n
       a(1:n,i)=2.0d0*a(1:n,i)-1.0d0
    end do
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
!余因子を求めるサブルーチン
  subroutine cal_cofactor(a,n,cof)
    double precision,intent(in) :: a(:,:)
	integer,intent(in) :: n
	double precision,intent(out) :: cof
	double precision,allocatable :: b(:,:),c(:,:)
	double precision :: det
    integer :: i,j
	!割付けを行う
    allocate(b(n-1,n-1),c(n-1,n))
    !取り除くi行目、j列目を設定する
    write(*,*) "Input i,j>=1"
    read(*,*) i,j
    if(i<1 .or. j<1) stop 'error (i<1 .or. j<1)'
    !i行目を除く
    c(1:i-1,1:n)=a(1:i-1,1:n)
    c(i:n-1,1:n)=a(i+1:n,1:n)
    !j列目を除く
    b(1:n-1,1:j-1)=c(1:n-1,1:j-1)
    b(1:n-1,j:n-1)=c(1:n-1,j+1:n)
	!確認用
    !write(*,*) "matrix c"
    !do i=1,n
    !   write(*,*) c(1:n-1,i)
    !end do
    write(*,*) "matrix b"
    do i=1,n-1
       write(*,'(100e12.4)') b(1:n-1,i)
    end do
	!余因子を求める　e:余因子 d:行列式
    det=b(1,1)*b(2,2)-b(1,2)*b(2,1)
    cof=(-1)**(i+j)*det
  end subroutine cal_cofactor
  
end module mat_subprogs

program main
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:)
	double precision :: cof
	integer :: n
	!行列の要素数nを設定する
    n=3
	!割付けを行う
	allocate(a(n,n))
	write(*,*) 'matrix A(n,n)'
    call allocate_rmat(a,n)
	call print_mat2(a) 
	call cal_cofactor(a,n,cof)
	write(*,'(a,100e12.4)') 'cofactor(A(i,j))= ',cof
end program main