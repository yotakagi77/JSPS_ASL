program p43_kashimura
 implicit none

 integer :: i,n
 double precision :: a,d
 double precision,allocatable :: r(:)
 
!データ数を決定
 write(*,*) "Input n>=1"
 read(*,*) n
 if(n<1) stop 'stop n<1'
 
!list2.5
 allocate (r(n))
 call random_seed
 call random_number(r(1:n))
 r(1:n)=2.0d0*r(1:n)-1.0d0
 write(*,*) r(1:n)
 !平均値
 a=0
 do i=1,n
  a=a+r(i)
 end do
 a=a/dble(n)
!標準偏差
 d=0
 do i=1,n
  d=d+(r(i)-a)**2
 end do
 d=SQRT(d/dble(n))
 
 write(*,*) "average=",a
 write(*,*) "standard_deviation=",d
 
stop
end program p43_kashimura

!サブルーチンなどで整理したいときは、内部手続き(外部手続き)に注意すること
!モジュールにするのも一興





 
