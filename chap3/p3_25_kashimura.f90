!p102 演習3.25
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
!3×3行列の行列式を計算するサブルーチン
  subroutine cal_det(a,n,det)
    double precision,intent(in) :: a(:,:)
    double precision :: det,tmp1,tmp2,tmp3
	integer :: n
	tmp1=a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)
	tmp2=a(1,3)*a(2,1)*a(3,2)-a(1,2)*a(2,1)*a(3,3)
	tmp3=-a(1,1)*a(2,3)*a(3,2)-a(1,3)*a(2,2)*a(3,1)
	det=tmp1+tmp2+tmp3
  end subroutine cal_det
!行列のある行、(ある列)をk倍するサブルーチン
  subroutine cal_1(a,b,n)
  double precision,allocatable,intent(in) :: a(:,:)
  double precision,allocatable,intent(out) :: b(:,:)
  double precision :: k
	integer :: i,n
	allocate(b(n,n))
    write(*,'(a)',advance='no') 'input i : '
	read(*,*) i
	if(i<1 .or. i>n) stop 'i must be 0<i<n+1'
	write(*,'(a)',advance='no') 'input coefficient k : '
	read(*,*) k
	if(k<1 .or. k>101) stop 'k must be 0<k<101'
	b=a
	b(1:n,i)=k*a(1:n,i)
  end subroutine cal_1
!行列のある2行、またはある2列を入れ替えるサブルーチン
  subroutine cal_2(a,c,n)
  double precision,intent(in) :: a(:,:)
  double precision,allocatable,intent(out) :: c(:,:)
	integer :: i,j,n
	allocate(c(n,n))
	write(*,*) "exchange A(i,1:n) and A(j,1:n)"
    write(*,'(a)',advance='no') 'input i,j : '
	read(*,*) i,j
	if(i<1 .or. i>n) stop 'i must be 0<i<n+1'
	if(j<1 .or. j>n) stop 'j must be 0<j<n+1'
	c(:,:)=a(:,:)
	c(1:n,j)=a(1:n,i)
	c(1:n,i)=a(1:n,j)
  end subroutine cal_2
!行列のある行(列)をk倍して他の行(列)に加えるサブルーチン
  subroutine cal_3(a,d,n)
  double precision,intent(in) :: a(:,:)
  double precision,allocatable,intent(out) :: d(:,:)
  integer :: i,j,n
  double precision :: k
	allocate(d(n,n))
	write(*,*) "add k*A(i,1:n) to A(j,1:n)"
    write(*,'(a)',advance='no') 'input i,j : '
	read(*,*) i,j
	if(i<1 .or. i>n) stop 'i must be 0<i<n+1'
	if(j<1 .or. j>n) stop 'j must be 0<j<n+1'
	write(*,'(a)',advance='no') 'input coefficient k : '
	read(*,*) k
	if(k<1 .or. k>101) stop 'k must be 0<k<101'
	d(:,:)=a(:,:)
	d(1:n,j)=a(1:n,j)+k*a(1:n,i)
  end subroutine cal_3
end module mat_subprogs



program main
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:),b(:,:),c(:,:),d(:,:)
	double precision :: det_a,det_b,det_c,det_d
	integer :: n
	!行列の要素数nを設定する
    n=3
	!割付けを行う
	allocate(a(n,n),b(n,n),c(n,n),d(n,n))
	!行列Aの設定
	write(*,*) 'matrix A(n,n)'
    call allocate_rmat(a,n)
	call print_mat2(a)
	call cal_det(a,n,det_a)
	write(*,'(a,100e12.4)') 'det(A)= ',det_a
	!基本定理1 行列のある行、ある列をk倍すると行列式もk倍
	write(*,*) 'matrix B'
	call cal_1(a,b,n)
	call print_mat2(b)
	call cal_det(b,n,det_b)
	write(*,'(a,100e12.4)') 'det(B)= ',det_b
	!基本定理2 行列のある2行、またはある2列を入れ替えると行列式の符号が反転する
	write(*,*) 'matrix C'
	call cal_2(a,c,n)
	call print_mat2(c)
	call cal_det(c,n,det_c)
	write(*,'(a,100e12.4)') 'det(C)= ',det_c
	!基本定理3 行列のある行(列)をk倍して他の行(列)に加えても行列式の値は変わらない
	call cal_3(a,d,n)
	write(*,*) 'matrix D'
	call print_mat2(d)
	call cal_det(d,n,det_d)
	write(*,'(a,100e12.4)') 'det(D)= ',det_d
end program main



!形状明示配列を多用しており、無駄に引数を増やしている問題がある