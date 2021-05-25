program p2_17
 implicit none
 integer,parameter::n=10
 real(8) a(n),am
 integer i,j
 call random_seed
 call random_number(a(:))
 do i=1,n-1
  do j=i+1,n
   if (a(i) > a(j)) then
    am=a(i)
    a(i)=a(j)
    a(j)=am
   end if
  end do
 end do
 write(*,*) a(1:n)
end program p2_17