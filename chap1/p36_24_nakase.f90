program ensyu24
implicit none
integer n, t, m, x
real(8) k, l, pai, ramda, sum, sum0, ondo, a, ex, s
open(1 , file = 'output1_24.d')

pai = 2.0d0 * acos(0.0d0)
k = 0.01d0
l = 1.0d0
write(* , *) ' input m '
read(* , *) m
sum = 0.0d0
! xをｍ分割
do x = 1, m+1
!　ｔを0～10まで繰り返し
 do t = 0, 10

 if ( t == 0) then
  ondo = l / x

 else if ( t > 0) then
! nを50まで繰り返し
  do n = 1, 50
   ramda = k * (n * pai / l)
   ex = exp(-ramda**2 * t) * (-1) ** (n-1) /(2 * n - 1)** 2
   s = sin(pai * x / l)
   a = ex * s
!　これいらんか
  sum0 = a
!ここで、1個前までの項の級数が残る
  sum = sum + sum0
  ondo = (4.0d0 * l / pai ** 2) * sum
  end do
  end if

  write(* , *) x, ondo
  write(1 , *) x, ondo
 end do
end do

close(1)
end program ensyu24