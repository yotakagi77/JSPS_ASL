!p108 演習3.34
module mat_subprogs      
  implicit none
contains
!n×n行列を乱数を用いて定義するサブルーチン
  subroutine allocate_rmat(a,n)
	double precision,allocatable,intent(out) :: a(:,:)
	integer :: i,n
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
	do i=1,n
	   write(*,'(100e12.4)') a(i,1:m)
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
!余因子を求めるサブルーチン
  subroutine cal_cofactor(a,i,j,cof)
    double precision,intent(in) :: a(:,:)
	integer,intent(in) :: i,j
	double precision,intent(out) :: cof
	double precision,allocatable :: b(:,:),c(:,:)
	double precision :: det
    integer :: n
	!割付けを行う
    allocate(b(n-1,n-1),c(n-1,n))
    !i行目を除く
    c(1:i-1,1:n)=a(1:i-1,1:n)
    c(i:n-1,1:n)=a(i+1:n,1:n)
    !j列目を除く
    b(1:n-1,1:j-1)=c(1:n-1,1:j-1)
    b(1:n-1,j:n-1)=c(1:n-1,j+1:n)
	!余因子を求める　e:余因子 d:行列式
    det=b(1,1)*b(2,2)-b(1,2)*b(2,1)
    cof=(-1)**(i+j)*det
  end subroutine cal_cofactor
!余因子行列に余因子を収めるサブルーチン
  subroutine allocate_cmat(a,b)
    double precision,intent(in) :: a(:,:)
	double precision,allocatable,intent(out) :: b(:,:)
	double precision :: cof
	integer :: i,j,n
	n=size(a,1)
	allocate(b(n,n))
	do j=1,n
	   do i=1,n
	     call cal_cofactor(a,i,j,cof)
		 b(j,i)=cof !添字が逆になることに注意
	   end do
	end do
  end subroutine allocate_cmat
  
end module mat_subprogs

program main
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:),b(:,:),c(:,:),e(:,:)
	double precision :: det,cof
	integer :: n
	!行列の要素数nを設定する
    n=3
	!行列A
	write(*,*) 'matrix A'
    call allocate_rmat(a,n)
	call print_mat2(a)
	!行列式det(A)\=0
	write(*,'(a)',advance='no') 'det(A)='
	call cal_det(a,n,det)
	write(*,*) det
	if(det==0) stop 'n must not be det=0'
	!余因子行列B
	write(*,*) 'matrix B'
	call allocate_cmat(a,b)
	call print_mat2(b)
	!逆行列C
	write(*,*) 'matrix C'
	allocate(c(n,n))
	c(:,:)=b(:,:)/det
	call print_mat2(c)
	!積が単位行列Eになるか確認
	write(*,*) 'matrix E'
	allocate(e(n,n))
	E(:,:)=matmul(a,c)
	call print_mat2(e)
end program main