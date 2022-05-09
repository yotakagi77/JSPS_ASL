program data
implicit none
real r, wa, sd, m, ave
allocatable :: r( : )
integer i, n
write(*,*) 'input n '
read(*,*) n
if(n<1) stop 'stop n<1'
allocate (r(n))
call random_seed
call random_number(r(1:n))
write(*,*) r(1:n)
wa = 0
do i = 1,n
wa = wa + r(i)
enddo

ave = wa / n

sd =0

do i = 1,n
m = m + (r(i) - ave) **2
enddo

sd = (m / n)**0.5

write(*,*) 'average =' , ave
write(*,*) 'standard deviation =' , sd

end program data