program ensyu21_2
implicit none
real(8) , allocatable :: a(: , :), b(: , :),  c(: , :), at(: , :), bt( : , :), ct( : , :), abt(: , :)
integer n, m, i, j, k
write(* ,*) ' input n, m (n == m) '
read(* , *) n, m
if (n  /= m) stop ' n == m'

!a,bを設定
allocate (a(n, m), b(n, m), c(n , m),  at(n , m), bt(n, m), ct(n, m), abt(n , m))
call random_seed
call random_number( a(: , :) )
call random_number( b(: , :) )


!組み込みでa,bを転置
at(: , :) = transpose(a)

bt(: , :) = transpose(b)

!組み込みで積を求める
abt(: , :) = matmul(bt, at)
do i = 1, n
 write(* , *) 'abt(:)= ', abt(i , 1:n)
end do

!組み込みで積の計算
c(: , :) = matmul(a , b)

!組み込みでｃを転置
ct(: , :) = transpose(c)
do i = 1, n
 write(* , *) 'ct(:)= ', ct(i , 1:n)
end do

end program ensyu21_2