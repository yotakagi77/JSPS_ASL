program p2_11
 implicit none
 real(8),allocatable::r(:)
 integer n
 write(*,*) 'Input n (>= 1)'
 read(*,*) n
 if (n < 1) then
  stop
 end if
 allocate(r(n))
 call random_seed
 call random_number(r(1:n))
 r(1:n)=10.0d0*r(1:n)
 write(*,*) int(r(1:n))
end program p2_11