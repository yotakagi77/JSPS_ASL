program matrix_product
implicit none
real a, b, c, d
allocatable :: a(:,:), b(:,:), c(:,:), d(:,:)
integer i, j, k, n, m, l

write(*,*) 'input n '
read(*,*) n
allocate (a(n,n))
allocate (b(n,n))
allocate (c(n,n))
allocate (d(n,n))
call random_seed
call random_number(a(1:n,1:n))
call random_number(b(1:n,1:n))

do j = 1,n
 do i = 1,n
  c(i,j) = 0.0
  do k = 1,n
   c(i,j) = c(i,j) + a(i,k) * b(k,j)
  enddo
 enddo
enddo

d(1:n,1:n) = matmul(a(1:n,1:n), b(1:n,1:n))

write(*,*) 'matrix_product of do loop =' 

do m = 1,n
write(*,*) c(m,1:n)
enddo

write(*,*) 'matrix_product of matmul =' 

do l = 1,n
write(*,*) d(l,1:n)
enddo

end program matrix_product
