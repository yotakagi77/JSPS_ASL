program p4_5
 use interface_mod
 implicit none
 integer, parameter::n=3
 real(8) a(1:n,1:n)
 integer i,j
 write(*,*) 'Input A'
 read(*,*) a(1:n,1:n)
 do i=1,n
  write(*,*) (a(i,j),j=1,n)
 end do
 write(*,*) chk_dd_mat(a,n)
end program p4_5