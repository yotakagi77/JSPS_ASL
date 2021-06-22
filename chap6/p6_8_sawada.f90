module subprogs
 implicit none
contains
 subroutine gaussian_elimination(a0,x,b,n)
    ! ---ガウスの消去法---
   integer, intent(in) :: n                     ! 配列の寸法
   real(8), intent(in) :: a0(n,n),b(n) ! 形状明示配列
   real(8), intent(out) :: x(n)        ! 形状明示配列
   integer i,k
   real(8) ar(n), a(n,n),d,alpha ! aは作業用の自動割付け配列
   a(:,:) = a0(:,:)            ! 係数配列a0をaにコピー
   x(:) = b(:)               ! 右辺ベクトルbをxにコピー
	!前進消去
   alpha = 1.0d0
   d = 1.0d0
   do k = 1,n
    if (a(k,k) == 0.0d0) stop ' pivot = 0' ! pivotが0なら停止する
    ar(k) = 1.0d0 / a(k,k)            ! arは対角成分の逆数
    a(k,k) = 1.0d0            ! 対角成分に1を設定
    a(k, k+1:n) = ar(k) * a(k, k+1:n) ! k行目のk+1列からn列にarを乗ずる
    x(k) = ar(k) * x(k)        ! k行の右辺要素にもarを乗ずる
    alpha = alpha/ar(k)
    d = d*a(k,k)
    do i = 1,n
     if (i > k) then   ! i行のk列からn列の要素とx(i)に対する演算
      a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
      x(i) = x(i) - a(i, k) * x(k)
      a(i, k) = 0.0d0
     end if
    end do
   end do
   
   write(*,*) '|A|=',alpha*d
 end subroutine gaussian_elimination

end module subprogs

program main
  use subprogs
  implicit none
  real(8), allocatable::a(:,:), b(:), x(:), r(:)
  integer :: n
  write(*,*) 'input n'
  read(*,*) n
  allocate (a(n,n),b(n),x(n),r(n))
  write(*,*) 'input A'
  read(*,*) a(1:n,1:n)
  call gaussian_elimination(a, x, b, n)
  r(:)=b(:)-matmul(a, x)
  write(*,*) 'Gauss-Jordan error = ', dot_product(r, r)
stop
end program main