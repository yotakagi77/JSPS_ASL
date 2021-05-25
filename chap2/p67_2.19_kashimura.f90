program p67_kashimura
implicit none

integer :: i,j,k,n
double precision,allocatable :: a(:,:),b(:,:),c(:,:),d(:,:)

  write(*,*) "Input n>=1"
  read(*,*) n
  if(n<1) stop 'error'
  
  allocate(a(n,n),b(n,n),c(n,n),d(n,n))
  
  write(*,*) "matrix a"
  call mat(a,n)
  write(*,*) "matrix b"
  call mat(b,n)
  
  do j=1,n
     do i=1,n
	    c(i,j)=0.0d0
		do k=1,n
		   c(i,j)=c(i,j)+a(i,k)*b(k,j)
		end do
	end do
  end do
 
  write(*,*) "matrix c =a*b" 
  do i=1,n
     write(*,*) c(1:n,i)
  end do
  
  write(*,*) "matrix d =a*b"
  d(:,:)=matmul(a(1:n,1:n),b(1:n,1:n))
  do i=1,n
     write(*,*) d(1:n,i)
  end do

stop
contains !内部手続き

!サブルーチン
subroutine mat(r,n)
 integer :: i,n
 double precision,allocatable :: r(:,:)
!乱数で埋める
 call random_seed
 do i=1,n
  call random_number(r(1:n,i))
  r(1:n,i)=2.0d0*r(1:n,i)-1.0d0
  write(*,*) r(1:n,i)
 end do
return
end
end program p67_kashimura