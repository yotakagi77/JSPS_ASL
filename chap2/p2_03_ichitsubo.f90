program  main
implicit none
integer n,i
real(kind(1d0)) ,allocatable :: a(:),b(:),c(:)

write(*,*) "input n:"
read(*,*) n

allocate(a(n),b(n),c(n))
call random_number(a)
call random_number(b)

c(:)=(a(:)-b(:))**2

write(*,*) "c(:)=(a(:)-b(:))**2=",c(:)

do i=1,n
    c(i)=(a(i)-b(i))**2
end do

write(*,*) "c(i)=(a(i)-b(i))**2=",c(:)

end program main