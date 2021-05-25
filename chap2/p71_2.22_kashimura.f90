program p71_kashimura
implicit none

integer :: i,j,n
double precision,allocatable :: a(:,:),b(:,:),c(:,:)

!nの値を設定する
  write(*,*) "Input n>=2"
  read(*,*) n
  if(n<2) stop 'error'
  allocate(a(n,n),b(n-1,n-1),c(n-1,n))
!取り除くi行目、j列目を設定する
  write(*,*) "Input i,j>=1"
  read(*,*) i,j
  if(i<1 .or. j<1) stop 'error'

!乱数を使ってn次正方行列を設定する  
  write(*,*) "matrix a"
  call mat(a,n)
  
!i行目を除く
  c(1:i-1,1:n)=a(1:i-1,1:n)
  c(i:n-1,1:n)=a(i+1:n,1:n)
!j列目を除く
  b(1:n-1,1:j-1)=c(1:n-1,1:j-1)
  b(1:n-1,j:n-1)=c(1:n-1,j+1:n-1)
  
  write(*,*) "matrix c"
  do i=1,n
   write(*,*) c(1:n-1,i)
  end do

  write(*,*) "matrix b"
  do i=1,n-1
   write(*,*) b(1:n-1,i)
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

end program p71_kashimura