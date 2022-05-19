module subprogs
 implicit none
contains
 function normal_vec2(v,n) result(nv)
 integer, intent(in) :: n
 real(8), intent(in) :: v(n)
 real(8) nv(n), vl
 vl = sqrt(dot_product(v,v))
 if(vl == 0.0d0) then
  nv(:) = 0.0d0
 else
  nv(:) = v(:) / vl
 endif
 end function normal_vec2

 function gs(a,n) result(e)
 integer  :: n
 real(8)  :: a(n,n)
 real(8) e(n,n), dotp
 integer k, j
 e(1:n,1) = normal_vec2(a(1:n,1:1),n)
 do k = 2,n
  e(1:n,k) = a(1:n,k)
  do j = 1,k-1
   dotp = dot_product(a(1:n,k),e(1:n,j))
   e(1:n,k) = e(1:n,k) - dotp * e(1:n,j)
  enddo
  e(1:n,k) = normal_vec2(e(1:n,k:k),n)
 enddo
 end function gs

end module subprogs

program main
 use subprogs
 implicit none
 integer :: i, n, j
 real(8), allocatable :: a(:,:), t(:,:), t_rev(:,:), b(:,:)
 write(*,*) 'input n'
 read(*,*) n
 allocate (a(n,n), t(n,n), t_rev(n,n), b(n,n) )
 call random_number(a)
 t = gs(a,n)
 do i = 1,n
  write(*,*) (t(i,j), j = 1,n)
 enddo
 t_rev = transpose(t)
 do i = 1,n
  write(*,*) (t_rev(i,j), j = 1,n)
 enddo
 write(*,*) 'TT_rev = '
 b = matmul(t,t_rev)
 do i = 1,n
  write(*,*) (b(i,j), j = 1,n)
 enddo
end program main

