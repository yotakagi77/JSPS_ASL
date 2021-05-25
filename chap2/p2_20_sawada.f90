program p2_20
 implicit none
 integer, parameter::n=5
 real(8) a(n,n),b(n,n),c(n,n),d(n,n)
 integer i,j
 call random_seed
 call random_number(a(1:n,1:n))
 call random_number(b(1:n,1:n))
 write(*,*)'a(n,n)='
 do i=1,n
  write(*,*) (a(i,j),j=1,n)
 end do
 write(*,*)'b(n,n)='
 do i=1,n
  write(*,*) (b(i,j),j=1,n)
 end do
 c(1:n,1:n)=a(1:n,1:n)*b(1:n,1:n)
 d(1:n,1:n)=matmul(a(1:n,1:n),b(1:n,1:n))
 write(*,*)'a*b='
 do i=1,n
  write(*,*) (c(i,j),j=1,n)
 end do
 write(*,*)'matmul(a,b)='
 do i=1,n
  write(*,*) (d(i,j),j=1,n)
 end do
end program p2_20