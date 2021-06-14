!p142 演習5.1
module subprogs
  implicit none
contains
  !三次元ベクトルを乱数を用いて定義するサブルーチン
  subroutine allocate_rvec(v,n)
	double precision,allocatable,intent(out) :: v(:)
	integer :: n
	allocate(v(n))
	call random_number(v)
	 v(1:n)=2.0d0*v(1:n)-1.0d0
  end subroutine allocate_rvec
  !外積(cross product)を求める関数
  function cross_product(u,v) result(w)
    double precision,intent(in) :: u(:),v(:)
	double precision,allocatable :: w(:)
    integer :: n
	  n=size(u)
	  if(n/=3) stop 'vector must be three-dimensional'
	  n=size(v)
	  if(n/=3) stop 'vector must be three-dimensional'
	  allocate(w(n))
	  w(:)=cshift(u,1)*cshift(v,2)-cshift(u,2)*cshift(v,1)
  end function cross_product
  !指定した大きさ(norm)のベクトルを返す関数
  function vnorm(vec,norm) result(nvec)
    double precision,intent(in) :: vec(:)
	double precision,intent(in),optional :: norm !optional属性を指定
	double precision :: nvec(size(vec))
	double precision :: vecl,factor
	vecl=dot_product(vec,vec)
	if(present(norm)) then !組み込み関数presentにより実引数の有無を確認
	   factor=norm
	else
	   factor=1.0d0
	end if
	if(vecl==0.0d0) then
	   nvec(:)=0.0d0 !ゼロベクトルの場合
	else
	   vecl=factor/sqrt(vecl) !ベクトルの大きさを
	   nvec(:)=vecl*vec(:)    !factorとする
	end if
  end function vnorm
  
  subroutine main
    double precision,allocatable :: a(:),b(:),v(:),nv(:)
	double precision :: norm
	integer :: n=3
	call allocate_rvec(a,n)
	write(*,*) 'vector A'
	write(*,'(100e12.4)') a(1:n)
	call allocate_rvec(b,n)
	write(*,*) 'vector B'
	write(*,'(100e12.4)') b(1:n)
	allocate(v(n),nv(n))
	v(:)=cross_product(a,b)
	write(*,*) 'cross_product A*B'
	write(*,'(100e12.4)') v(1:n)
	write(*,'(a)',advance='no') 'alpha='
	read(*,*) norm
	nv(:)=vnorm(v,norm)
	write(*,*) 'vector NV'
	write(*,'(100e12.4)') nv(1:n)
  end subroutine main
end module subprogs



program p5_1_kashimura
  use subprogs
  implicit none
  call main
stop
end program p5_1_kashimura