program ensyu12
implicit none
real(8) a(10), r
integer i, n
 do i = 1, 10
 a(1) = 16.0
 r = 0.8
 a(i) = a(1) * r ** (i-1)
 write(* , *) i,a(i)
 end do
end program ensyu12