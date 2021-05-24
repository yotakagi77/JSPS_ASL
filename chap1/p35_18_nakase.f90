program ensyu18
implicit none
real(8) a(10), r
integer i, n
open( 1, file= 'output.d' )
 do i = 1, 10
 a(1) = 16.0
 r = 0.8
 a(i) = a(1) * r ** (i-1)
 write(1 , *) i,a(i)
 end do
write( 1, *) ' '
close(1)
end program ensyu18