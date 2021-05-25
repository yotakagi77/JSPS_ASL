program ensyu21
implicit none
real(8) , allocatable :: a(: , :), b(: , :), c(: , :), ct(: , :), at(: , :), bt( : , :), abt(: , :)
integer n, m, i, j, k
write(* ,*) ' input n, m (n == m) '
read(* , *) n, m
if (n  /= m) stop ' n == m'

!a,bを設定
allocate (a(n, m), b(n, m), c(n , m), ct(n , m), at(n , m), bt(n, m), abt(n , m))
call random_seed
call random_number( a(: , :) )
write(* , *) 'a'
do i = 1 , m
write(* , *) a(i ,  1:m)
end do
call random_number( b(: , :) )
write(* , *) 'b'
do i = 1, n
 write(* , *) ' b(:)=', b(i , 1 : n)
end do

!a、bを転置
do i = 1, n
 do j = 1, n
 at( i, j ) = a( j , i )
 bt( i ,j ) = b( j , i)
 end do
end do

write(* , *) 'at'
do i = 1, n
write( * , *)at(i , 1 : n)
end do

write(* , *) 'bt'
do i = 1, n
write( * , *)bt(i , 1 : n)
end do 

!右辺の答え
do j = 1, n
 do i = 1, n
  abt(i, j) = 0.0d0
  do k = 1, n
   abt(i , j) = abt(i , j) + bt(i , k) * at(k , j)
  end do
 end do
end do
write(* , *)'abt'
do i = 1, n
 write(* , *) abt(i , 1:n)
end do


!左辺積の計算
do j = 1, n
 do i = 1, n
  c(i, j) = 0.0d0
  do k = 1, n
   c(i , j) = c(i , j) + a(i , k) * b(k , j)
  end do
 end do
end do

write(* , *)'c'
do i = 1, n
 write(* , *) c(i , 1:n)
end do

!左辺の答え
do i = 1, n
 do j = 1, n
 ct( i, j ) = c( j , i )
 end do
end do
write(* , *) 'ct'
do i = 1, n
write( * , *)ct(i , 1 : n)
end do

end program ensyu21