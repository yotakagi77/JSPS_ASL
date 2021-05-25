program ensyu9
implicit none
real(8), allocatable :: u( : ), v( : )
integer :: n
open(1, file='input9.txt')
open(2, file='output9.txt')
!読み取り
read(1, *) n
allocate (u(n), v(n))
!読み取り
read(1 , *) u(1 : n)
read(1 , *) v(1 : n)
close(1)
write(2 , *) 'dp=' , dot_product(u , v)
close(2)
deallocate (u , v)

end program ensyu9