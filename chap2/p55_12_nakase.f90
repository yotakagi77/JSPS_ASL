program ensyu12
implicit none
real(8), allocatable ::  x( : ), y( : ), r( : )
integer n, i, a

open( 1, file='output12.d')
write(* , *) 'input n '
read(* , *) n

!r読み取り
allocate (x(n), y(n), r(n))
!rの設定
call random_seed
call random_number(r(1:n))
r(1:n) = 2.0d0 * r(1:n) - 1.0d0
write(1 , *) 'r'
do i = 1, n
 write(1 , *) r(i)
end do

!xを出す
a = 10 / (n-1)
write(1 , *) 'x, y'
do i = 1, n
 x(i) = a * i - a
 y(i) = 2 * x(i) + 1.0d0 + r(i)
 write(1 , *) x(i), y(i)
end do

close(1)
deallocate(x, y, r)
end program ensyu12