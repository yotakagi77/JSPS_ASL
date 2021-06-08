!p108 演習3.31
module vec_subprogs      
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
!外積(cross product)を求めるサブルーチン
  subroutine cal_cross_product_2_7(u,v,w)
    double precision,intent(in) :: u(:),v(:)
	double precision,allocatable,intent(out) :: w(:)
    integer :: n
	  n=size(u)
	  allocate(w(n))
	  w(:)=cshift(u,1)*cshift(v,2)-cshift(u,2)*cshift(v,1)
  end subroutine cal_cross_product_2_7
  subroutine cal_cross_product_3_21(u,v,w)
    double precision,intent(in) :: u(:),v(:)
	double precision,allocatable,intent(out) :: w(:)
	double precision,allocatable :: tmp1(:,:),tmp2(:,:),tmp3(:,:)
	double precision :: det
    integer :: i,j,n
	  n=size(u)
	  allocate(w(n),tmp1(2,n),tmp2(2,n),tmp3(2,n-1))
	  tmp1(1,1:n)=u(:)
	  tmp1(2,1:n)=v(:)
	  do i=1,n
	     tmp2(:,:)=cshift(tmp1,i,2)
		 !write(*,*) i
		 !write(*,'(100e12.4)') tmp2(1,:)
		 !write(*,'(100e12.4)') tmp2(2,:)
		 tmp3(:,:)=tmp2(1:2,1:2)
		 w(i)=tmp3(1,1)*tmp3(2,2)-tmp3(1,2)*tmp3(2,1)
	  end do
  end subroutine cal_cross_product_3_21
end module vec_subprogs



program main
  use vec_subprogs
  implicit none
    double precision,allocatable :: a(:),b(:),c(:),d(:)
	integer :: n
	!要素数nの設定
	n=3
	!ベクトルAの設定
	write(*,*) 'vector A(3)'
    call allocate_rvec(a,n)
	write(*,'(100e12.4)') a(:)
	!ベクトルBの設定
	write(*,*) 'vector B(3)'
    call allocate_rvec(b,n)
	write(*,'(100e12.4)') b(:)
	!A,Bの外積を計算
	write(*,*) 'cross product A*B= C by p2.7'
	call cal_cross_product_2_7(a,b,c)
	write(*,'(100e12.4)') c(:)
	write(*,*) 'cross product A*B= D by p3.21'
	call cal_cross_product_3_21(a,b,d)
	write(*,'(100e12.4)') d(:)
end program main