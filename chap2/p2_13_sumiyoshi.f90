program symmetric
implicit none
real a
allocatable :: a(:,:)
integer n, i, j, k
write(*,*) 'input n '
read(*,*) n
allocate (a(n,n))
call random_seed
call random_number(a(1:n,1:n))

do i = 1,n
 do j = 1,n
  a(i,j) = a(j,i)
 enddo
enddo

do k =1,n
write(*,*)a(k,1:n)
enddo

end program symmetric