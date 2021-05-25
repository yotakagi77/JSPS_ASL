program p58_kashimura
 implicit none

 integer :: i,n
 double precision,allocatable :: r(:,:)
 
!データ数を決定
 write(*,*) "Input 100>=n>=1"
 read(*,*) n
 if(n<1 .or. 100<n) stop 'error'
! allocate (r(n,n))
 
!サブルーチンの読み込み
 call matrix(r,n)

stop
contains !内部手続き

!サブルーチン
subroutine matrix(r,n)
 integer :: i,n
 double precision,allocatable :: r(:,:)
!乱数で埋める
 allocate (r(n,n))
 call random_seed
 do i=1,n
  call random_number(r(1:n,i))
  r(1:n,i)=2.0d0*r(1:n,i)-1.0d0
  write(*,*) r(1:n,i)
 end do
return
end

end program p58_kashimura