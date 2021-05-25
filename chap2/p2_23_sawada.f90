program p2_23
 implicit none
 real d,e
 integer n,i,j
 real(8),allocatable::a(:,:),b(:,:),c(:,:)
 n=3
 allocate(a(n,n),b(n-1,n-1),c(n-1,n))
 call random_seed
 call random_number(a(1:n,1:n))
 write(*,*)'a(n,n)='
 do i=1,n
  write(*,*) a(1:n,i)
 end do
 write(*,*)'Input i and j'
 read(*,*) i,j
 c(1:i-1,1:n)=a(1:i-1,1:n)
 c(i:n-1,1:n)=a(i+1:n,1:n)
 b(1:n-1,1:j-1)=c(1:n-1,1:j-1)
 b(1:n-1,j:n-1)=c(1:n-1,j+1:n)
 write(*,*)'b(n-1,n-1)='
 do i=1,n-1
  write(*,*) b(1:n-1,i)
 end do
 d=b(1,1)*b(2,2)-b(1,2)*b(2,1)
 e=(-1)**(i+j)*d
 write(*,*)'cof=',e
end program p2_23