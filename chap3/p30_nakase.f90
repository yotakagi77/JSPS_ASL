module subprog
 implicit none
contains
!仮配列はaで配列bを返す
 function tenti(a, n) result(b)
 integer, intent(in) :: n
 real(8), intent(in) :: a(n, n)
 real(8) b(n, n)
 integer i, j
!転置行列bを作る
 do i = 1, n
  do j = 1, n
   b(i, j) = a(j, i)
  end do
 end do
!最後に出力する
 do i = 1, n
  write(*,*)b(i, 1:n)
 end do
 end function tenti
end module subprog

program ensyu30
 use subprog
 implicit none
 real(8), allocatable :: x(: , :), y(:, :), z(:, :)
 integer n, i
 write(*,*) 'input n'
 read(*,*) n
 allocate(x(n, n), y(n, n), z(n, n))
 call random_number(x(n, n))
 do i = 1, n
  write(*,*) 'x', x(i, 1:n)
 end do
 !組み込み関数で転置させる
 z( : , :) = transpose(x)
 do i = 1, n
  write(*,*) z(i, 1:n)
 end do
 y = tenti(x, n)
end program ensyu30