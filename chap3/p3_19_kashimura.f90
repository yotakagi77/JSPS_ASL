!p101 演習3.19
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
!対角要素の和(トレース)を求めるサブルーチン
  subroutine cal_tr(a,tr)
    double precision,intent(in) :: a(:,:)
    double precision,intent(out) :: tr
	integer :: i,n
    n=size(a,1)
	tr=0
	do i=1,n
	   tr=tr+a(i,i)
	end do
  end subroutine cal_tr
  
end module mat_subprogs

program random_mat
  use mat_subprogs
  implicit none
    double precision,allocatable :: a(:,:),b(:,:),ab(:,:),ba(:,:)
	double precision :: tr_a,tr_ab,tr_ba
	integer :: n
	!行列の要素数nを設定する
	write(*,'(a)',advance='no') 'input n : '
	read(*,*) n
	if(n<1 .or. n>100) stop 'n must be 0<n<101'
	!割付けを行う
	allocate(a(n,n),b(n,n),ab(n,n),ba(n,n))
	!行列Aのトレースが求められていることを確認
	write(*,*) 'matrix A(n,n)'
    call allocate_rmat(a,n)
	call print_mat2(a)
	call cal_tr(a,tr_a)
	write(*,*) 'tr(A)= ',tr_a
	!行列Bを定義する
	write(*,*) 'matrix B(n,n)'
    call allocate_rmat(b,n)
	call print_mat2(b)
	!行列ABと行列BAを計算
	write(*,*) 'matrix AB(n,n)'
	ab(:,:)=matmul(a,b)
	call print_mat2(ab)
	write(*,*) 'matrix BA(n,n)'
	ba(:,:)=matmul(b,a)
	call print_mat2(ba)
	!行列ABと行列BAのトレースを計算
	call cal_tr(ab,tr_ab)
	write(*,*) 'tr(AB)= ',tr_ab
	call cal_tr(ba,tr_ba)
	write(*,*) 'tr(BA)= ',tr_ba
end program random_mat

!allocateをサボるな！