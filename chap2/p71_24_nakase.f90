program ensyu24
implicit none
real(8) a( 3 ), b( 3 ), c( 2, 3), x,y,z
integer i

call random_seed
call random_number( a(:) )
call random_number( b(:) )

!a,bをcに格納
c(1, 1:3) =a (1:3)
c(2, 1:3) = b(1 :3)
do i = 1, 2
 write( * , *) 'c(:)= ', c( i, 1:3)
end do

x = a(2) * b(3) - a(3) * b(2)
y = a(3) * b(1) - a(1) * b(3)
z = a(1) * b(2) - a(2) * b(1)
write(* , *) 'x, y, z'
write(* , *) x, y, z
end program ensyu24