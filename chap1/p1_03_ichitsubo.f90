program ensyu
implicit none
integer i,a(1:10)

a(1)=1
a(2)=2

do i=1,10
if (i>=3) THEN
a(i)=a(i-1)+a(i-2)
end if
write(*,*) a(i)

end do

end program ensyu
