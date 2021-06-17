!p156 演習6.1
module subprogs
  implicit none
contains
  subroutine gauss_jordan(a0,x,b,n)
    ! ---ガウス-ジョルダン法(部分pivot選択なし)---
	integer, intent(in) :: n                     ! 配列の寸法
	double precision, intent(in) :: a0(n,n),b(n) ! 形状明示配列
	double precision, intent(out) :: x(n)        ! 形状明示配列
	integer i,k
	double precision ar, a(n,n) ! aは作業用の自動割付け配列
	a(:,:) = a0(:,:)            ! 係数配列a0をaにコピー
	x(:)   = b(:)               ! 右辺ベクトルbをxにコピー
	do k = 1, n
	   if (a(k,k) == 0.0d0) stop ' pivot = 0' ! pivotが0なら停止する
	   ar = 1.0d0 / a(k,k)            ! arは対角成分の逆数
	   a(k,k)      = 1.0d0            ! 対角成分に1を設定
	   a(k, k+1:n) = ar * a(k, k+1:n) ! k行目のk+1列からn列にarを乗ずる
	   x(k)        = ar * x(k)        ! k行の右辺要素にもarを乗ずる
	   do i = 1, n
	      if (i /= k) then   ! i行のk列からn列の要素とx(i)に対する演算
		     a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
			 x(i)        = x(i)        - a(i, k) * x(k)
			 a(i, k    ) = 0.0d0
		  end if
	   end do
	end do
  end subroutine gauss_jordan

  subroutine gaussian_elimination(a0,x,b,n)
    ! ---ガウスの消去法---
	integer, intent(in) :: n                     ! 配列の寸法
	double precision, intent(in) :: a0(n,n),b(n) ! 形状明示配列
	double precision, intent(out) :: x(n)        ! 形状明示配列
	integer i,k
	double precision ar, a(n,n) ! aは作業用の自動割付け配列
	a(:,:) = a0(:,:)            ! 係数配列a0をaにコピー
	x(:)   = b(:)               ! 右辺ベクトルbをxにコピー
	!前進消去
	do k = 1, n
	   if (a(k,k) == 0.0d0) stop ' pivot = 0' ! pivotが0なら停止する
	   ar = 1.0d0 / a(k,k)            ! arは対角成分の逆数
	   a(k,k)      = 1.0d0            ! 対角成分に1を設定
	   a(k, k+1:n) = ar * a(k, k+1:n) ! k行目のk+1列からn列にarを乗ずる
	   x(k)        = ar * x(k)        ! k行の右辺要素にもarを乗ずる
	   do i = 1, n
	      if (i > k) then   ! i行のk列からn列の要素とx(i)に対する演算
		     a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
			 x(i)        = x(i)        - a(i, k) * x(k)
			 a(i, k    ) = 0.0d0
		  end if
	   end do
	end do
	!後退代入
	do i = n, 1 ,-1
	   x(i) = x(i) - dot_product(a(i,i+1:n), x(i+1:n))
	end do
  end subroutine gaussian_elimination
  
  subroutine set_random_ab(a, b, x, n)
    ! nを取得しa,b,xを割付け、aとbに乱数を設定
	integer,intent(out) :: n
	double precision, allocatable, intent(out) :: a(:,:), b(:), x(:) ! 未割付け配列
	integer :: i
	write(*,'(a)',advance='no') ' input n : '
    read(*,*) n
    if(n<1 .or. n>100) stop 'n must be 0<n<100'
	allocate (a(n,n),b(n),x(n))
	call random_seed
	call random_number(a)
	call random_number(b)
	do i = 1,n
       a(1:n, i)= 2.0d0 * a(1:n, i) - 1.0d0
    end do
	b(1:n) = 2.0d0 * b(1:n) - 1.0d0
  end subroutine set_random_ab
  
  subroutine print_mat2(a)
    double precision,intent(in) :: a(:, :)
	integer :: i,n,m
	m=size(a,1) !組み込み関数sizeによりaの最初の次元の寸法を得る
	n=size(a,2) !組み込み関数sizeによりaの二番目の次元の寸法を得る
	do i=1,m
	   write(*,*) a(i, 1:n)
	end do
  end subroutine print_mat2
end module subprogs



program main
  use subprogs
  implicit none
  double precision,allocatable :: a(:, :), b(:), x(:), r(:)
  integer :: n
  call set_random_ab(a, b, x, n)
  call gaussian_elimination(a, x, b, n)
  allocate (r(n))
  r(:)=b(:)-matmul(a, x)
  write(*,*) 'Gauss-Jordan error = ', dot_product(r, r)
  deallocate(a, b, x)
stop
end program main