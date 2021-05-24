program ensyu1
implicit none
    integer a(20), i
     a(1)=1
     a(2)=1
     do i=3, 10
       a(i)=a(i-1)+a(i-2)
     end do
     do i=1, 10
     write(*,*) a(i)
     end do
stop
end program ensyu1