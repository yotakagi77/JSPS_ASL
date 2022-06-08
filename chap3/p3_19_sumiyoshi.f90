module subprog
 implicit none
contains
 function func_tr(a) result(tr)
  integer :: i
  real(8), allocatable, intent(in) :: a(:,:)
  real(8) :: tr
  tr = 0
  do i = 1, size(a,1)
   tr = tr + a(i,i)
  enddo
 end function func_tr 
end module subprog

program main
 use subprog
 implicit none
 real(8), allocatable :: a(:,:), b(:,:), c(:,:), d(:,:)
 integer :: i, j, n
 write(*,*) 'input n'
 read(*,*) n
 allocate (a(n,n))
 do i = 1, n
   do j = 1, n 
    read(*,*) a(i,j)
   enddo
 enddo

 allocate (b(n,n))
 do i = 1, n
   do j = 1, n 
    read(*,*) b(i,j)
   enddo
 enddo

 do i = 1, n
   write(*,*) (a(i,j), j = 1,n)   
 enddo

 do i = 1, n
   write(*,*) (b(i,j), j = 1,n)   
 enddo
 c = matmul(a,b)
 d = matmul(b,a)
 
 write(*,*) 'tr(AB) =' , func_tr(c)
 write(*,*) 'tr(BA) =' , func_tr(d)

if(func_tr(c) == func_tr(d)) then
 write(*,*) 'tr(AB) = tr(BA)'
else
 write(*,*) 'tr(AB) /= tr(BA)'
endif 
end program main
   
