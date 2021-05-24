program ensyu15
implicit none
real(8) :: x1, x2, a, er, er0 = 1.0d-6
integer :: i, k, im = 1000
write(* , *) ' input k , a '
read(* , *) k, a
if ( k <= 0 .or. a <= 0.0d0 ) stop ' k <= 0 or a <= 0.0d0'
x1 = a
do i = 1, im
 x2 = x1 - 1d0/k * (x1**k-a) / x1**(k-1)
 er = abs(x2 - x1)
 if ( er < er0 ) exit
 x1 = x2
end do
write(* , *) x2
end program ensyu15