program p2_14
 implicit none
 integer i,j,k,is,n
 real(8) t1,t2
 integer, allocatable::a(:,:,:)
 write(*,*) 'Input n:'
 read(*,*) n
 allocate(a(1:n,1:n,1:n), stat=is)
 if (is /= 0) then
  stop
 end if
 call cpu_time(t1)
 do k=1,n
  do j=1,n
   do i=1,n
    a(i,j,k)=0
   end do
  end do
 end do
 call cpu_time(t2)
 write(*,*) 'cpu time =',t2-t1
 call cpu_time(t1)
 do i=1,n
  do j=1,n
   do k=1,n
    a(i,j,k)=0
   end do
  end do
 end do
 call cpu_time(t2)
 write(*,*) 'cpu time =',t2-t1
 deallocate(a)
end program p2_14