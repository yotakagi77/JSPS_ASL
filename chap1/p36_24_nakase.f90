program ensyu24
implicit none
integer n, t, m, r
real(8) k, l, pai, ramda, sum, sumi, ondo, ex, s, x
character (len=3) :: fno
character (len=40) :: fname

pai = 2.0d0 * acos(0.0d0)
k = 0.01d0
l = 1.0d0

write(* , *) ' input m '
read(* , *) m

do t = 0 , 10
write(fno, fmt='(i3.3)')t
fname = 'output '//fno// ' .d'
! xをｍ分割
 do r = 1, m+1
  x =  l / m * r - l / m
  if ( t == 0) then
   if ( x <= l / 2) then
    ondo = x
   else if ( l /2 < x .AND. x <= l ) then
    ondo = l - x
   end if

 else if ( t > 0) then
! nを50まで繰り返し
  sum = 0.0d0
  do n = 1, 50
!λの式
   ramda = k * (n * pai / l)
!expの部分
   ex = (exp(-ramda**2 * t) * (-1) ** (n-1)) / (2 * n - 1)** 2
!sin の部分
   s = sin(pai * x / l)
!sin と expをかける
   sumi = ex * s
!ここで、1個前までの項の級数が残る
   sum = sum + sumi
   ondo = (4.0d0 * l / pai ** 2) * sum
  end do
  end if
  open(t, file='output '//fno// ' .d')
  write(t , *) x, ondo
 end do
 close(t)
end do
end program ensyu24