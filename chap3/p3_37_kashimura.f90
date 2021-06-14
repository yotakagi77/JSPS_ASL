!p112 演習3.37
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
	do i=1,n
	   write(*,'(100e12.4)') a(i,1:m)
	end do
  end subroutine print_mat2
!3×3行列の行列式を計算する関数
  double precision function det(a)
    double precision,intent(in) :: a(:,:)
    double precision :: tmp1,tmp2,tmp3
	integer :: n
	n=size(a,1)
	if(n/=3) stop 'n must be 3'
	tmp1=a(1,1)*a(2,2)*a(3,3)+a(1,2)*a(2,3)*a(3,1)
	tmp2=a(1,3)*a(2,1)*a(3,2)-a(1,2)*a(2,1)*a(3,3)
	tmp3=-a(1,1)*a(2,3)*a(3,2)-a(1,3)*a(2,2)*a(3,1)
	det=tmp1+tmp2+tmp3
  end function det
!正規化ベクトルを返す関数
  function normal_vec(v,n) result(nv)     !仮配列はv、関数が返す配列はnv
    integer,intent(in) :: n
    double precision,intent(in) :: v(n)  !仮配列vは形状明示仮配列
	double precision :: nv(n),v1         !配列nvの寸法はsizeをもちいて定める
	v1=sqrt(dot_product(v,v))            !ベクトルの大きさを計算
	if(v1==0.0d0) then
	  nv(:)=0.0d0                        !v1=0のときはゼロベクトルを返す
	else
	  nv(:)=v(:)/v1                      !配列要素を正規化してnvに格納
	end if
  end function normal_vec
!正則行列aの列ベクトルを正規直交化して、eの各列に格納して返す
  function gs(a,n) result(e)
  integer,intent(in) :: n
  double precision,intent(in) :: a(n,n) !形状明示仮配列
  double precision :: e(n,n),dotp
  integer :: k,j
  e(1:n,1)=normal_vec(a(1:n,1:1),n) !e1を定める
  do k=2,n
     e(1:n,k)=a(1:n,k)
	 do j=1,k-1
	    dotp=dot_product(a(1:n,k),e(1:n,j))
		e(1:n,k)=e(1:n,k)-dotp*e(1:n,j)
	end do
	e(1:n,k)=normal_vec(e(1:n,k:k),n) !ekを正規化する
  end do
end function gs
end module mat_subprogs



program main
  use mat_subprogs
  implicit none
    integer :: n
    double precision,allocatable :: a(:,:),t(:,:),tt(:,:),e(:,:)
!行列の要素数nを設定する
    n=3
	allocate(a(n,n),t(n,n),tt(n,n),e(n,n))
!行列Aの設定
	write(*,*) 'matrix A'
    call allocate_rmat(a,n)
	call print_mat2(a)
!行列式det(A)\=0 正則の確認
	write(*,'(a)',advance='no') 'det(A)='
	write(*,'(100e12.4)') det(a)
	if(det(a)==0) stop 'det(a) must not be 0'
!直交行列Tの計算
    write(*,*) 'orthogonal matrix T'
    t(:,:)=gs(a,n)
	call print_mat2(t)
	write(*,'(a)',advance='no') 'det(T)='
	write(*,'(100e12.4)') det(t)
!転置行列TTの計算
	write(*,*) 'transposed matrix TT'
	tt(:,:)=transpose(t)
	call print_mat2(tt)
!T*TT=Eとなるか確認
    write(*,*) 'identity matrix E'
	e(:,:)=matmul(tt,t)
	call print_mat2(e)
  stop
end program main