program ensyu18
implicit none
real(8), allocatable :: a(: , :), b(: , :), c(: , :), x(:), y(:)
integer i, j, k, n
real(8) t1, t2, t3, t4
write(* ,*) 'input n'
read(* ,*) n

!a, x の要素の決定
allocate( a(n , n), b(n , n), c(n , n), x(n), y(n))
call random_seed
call random_number( a(:, :))

call random_seed
call random_number( x(:))

!行列ベクトル積
call cpu_time(t1)
do i = 1, n
 y(i) = 0.0d0
 do j = 1, n
  y(i) = y(i) + a(i , j) * x(j)
 end do
end do
call cpu_time(t2)

write(* , *) ' cpu time = ' , t2-t1

!2次元配列a,bの設定
call random_seed
call random_number( b(:, :))

!行列積
call cpu_time(t3)
do j = 1, n
 do i = 1, n
  c(i , j) = 0.0d0
  do k = 1, n
   c(i , j) = c(i , j ) + a ( i , k) * b (k , j)
  end do
 end do
end do
call cpu_time(t4)
write(* , *)'cpu time=', t4-t3


end program ensyu18