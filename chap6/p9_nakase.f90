module subprog
 implicit none
contains
 subroutine kansu(x, r, n, y) 
 integer, intent(in) :: n
 real(8), intent(in) ::  r(n)
 real(8) y(n), x(n)
 integer i, a
 open(1, file='output9.txt')
 write(*,*) 'x  y'
 a = 20 / (n-1)
 do i = 1, n
  x(i) =  -10 + a*(i-1)
  y(i) = 0.1*x(i)**3 + 0.2*x(i)**2 + 0.5*x(i)+ r(i)
  write(1 , *) x(i), y(i)
 end do
 close(1)
 end subroutine kansu
end module subprog

program ensyu9
use subprog
 implicit none
 real(8), allocatable ::  x( : ), y( : ), r( : )
 integer n, i, a
!分割数の読み込み
 write(* , *) 'input n '
 read(* , *) n
 allocate (x(n), y(n), r(n))
!rの設定
 call random_seed
 call random_number(r(1:n))
 r(1:n) = 2.0d0 * r(1:n) - 1.0d0
 call kansu(x, r, n, y)
end program ensyu9