!p 演習5.4
!p145 list5.4
module mat_subprogs
  implicit none
contains
  subroutine print_mat(a)
  implicit none
    double precision,intent(in) :: a(:,:) !形状引継ぎ配列
	integer :: i,n,m
	n=size(a,1)
	m=size(a,2)
	do i=1,n
	   write(*,'(100e12.4)') a(i,1:m)
	end do
  end subroutine print_mat
  recursive function det_mat(a,n) result(det)
    integer,intent(in) :: n               !nは配列の寸法
	double precision,intent(in) :: a(n,n) !形状明示仮配列を利用
	double precision :: det,b(n-1,n-1)
	integer :: i
	if(n>1) then
	   det=0.0d0
	   !---余因子展開により、第1行で展開して行列式を計算---
	   do i=1,n
	      !---aの1行とi列目をの除く小行列を計算---
		  b(1:n-1,1:i-1)=a(2:n,1:i-1)
		  b(1:n-1,i:n-1)=a(2:n,i+1:n)
		  det=det+(-1.0d0)**(i+1)*a(1,i)*det_mat(b,n-1)
	   end do
    else
	   det=a(1,1)
	end if
  end function det_mat
end module mat_subprogs



program cal_det
use mat_subprogs
implicit none
  integer,parameter :: n=5
  double precision :: a(n,n)
  call random_number(a)
  write(*,*) 'vector A'
  call print_mat(a)
  write(*,'(a)',advance='no') 'det(A)='
  write(*,'(100e12.4)') det_mat(a,n)
stop
end program cal_det