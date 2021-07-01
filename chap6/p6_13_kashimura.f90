!P166 演習6.13
module subprogs
  implicit none
contains
  
  subroutine sqr(a,b,x,n,itrmax,er0)
    ! a=係数行列,b=右辺ベクトル,itrmax=最大反復回数,er0=誤差の閾値
	integer,intent(in) :: n,itrmax
	double precision,intent(in) :: a(n,n),b(n),er0
	double precision,intent(out) :: x(n)
	double precision :: s,er,rd(n),r(n),omg=1.0d0
	integer :: i,itr
	! 対角要素が0でなければその逆数をrdとする
	do i=1,n
	   if(a(i,i)==0.0d0) stop 'a(i,i)==0.0d0'
	   rd(i)=1.0d0/a(i,i)
	end do
	x(1:n)=0.0d0  ! 初期解を0とする
	do itr=1,itrmax  ! 反復計算のループ(最大itrmax回の反復)
	   !上三角行列に初期値xを与えて次のxを計算
	   do i=1,n
	      s=dot_product(a(i,1:i-1),x(1:i-1))
	      s=s+dot_product(a(i,i+1:n),x(i+1:n))
	      x(i)=x(i)+omg*(rd(i)*(b(i)-s)-x(i))
	   end do
	   r(1:n)=b(1:n)-matmul(a,x)  ! 残差ベクトル
	   er=dot_product(r,r)        ! 残差ベクトルの内積を誤差erとする
	   write(*,*) 'itr=',itr,'err=',er  ! 途中経過の出力
	   if(er<=er0) then  ! 誤差が閾値er0以下なら反復計算終了
	      write(*,*) '# converged #'
		  exit  ! 収束した場合には反復計算のループから抜ける
       end if
	end do
  end subroutine sqr
  
  subroutine alloc_dd_mat(a,b,x,n)
    ! nを取得しa,b,xを割付け、aとbに乱数を設定
	integer,intent(out) :: n
	double precision, allocatable, intent(out) :: a(:,:), b(:), x(:) ! 未割付け配列
	double precision :: rd
	integer :: i,j
	write(*,'(a)',advance='no') ' input n : '
    read(*,*) n
    if(n<1 .or. n>100) stop 'n must be 0<n<100'
	allocate (a(n,n),b(n),x(n))
	call random_seed
	call random_number(a)
	call random_number(b)
	! --- 対角優位行列Aの設定 ---
	do i=1,n
	   rd=0.1d0  !0出ないとき、狭義の対角優位行列
       a(i,1:n)= 2.0d0 * a(i,1:n) - 1.0d0
	   do j=1,n
	      if(i/=j) then
	         rd=rd+sqrt(a(i,j)**2)
	      end if
	   end do
	   rd=1/rd
	   a(i,1:n)=rd*a(i,1:n)
	   a(i,i)=1.0d0
    end do
	! --- 行列Bの設定 ---
	b(1:n) = 2.0d0 * b(1:n) - 1.0d0
  end subroutine alloc_dd_mat
  
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
  double precision,allocatable :: a(:,:),b(:),x(:)
  integer :: n ,itrmax=100
  double precision :: er0=1.0d-6
  call alloc_dd_mat(a,b,x,n)  !係数行列a,右辺ベクトルbの設定など
  call sqr(a,b,x,n,itrmax,er0)  !SQR法
  ! ... 数値解xの出力などを行う
  write(*,'(a)',advance='no') ' x : '
  write(*,*) x(:)
end program main