function chk_dd_mat(a,n) result(x)
 implicit none
 integer, intent(in)::n
 real(8), intent(in)::a(1:n,1:n)
 real(8) sum,aij
 integer i,j,x
 do i=1,n
  sum=0
  do j=1,n
   sum=sum+abs(a(i,j))
  end do
  aij=sum-abs(a(i,i))
  if (abs(a(i,i)) <= aij) stop '0'
 end do
 x=1
end function chk_dd_mat