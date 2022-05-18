module subprogs
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

 function func_tr_seki(a,b) result(tr)
  integer :: i, j
  real(8), allocatable, intent(in) :: a(:,:),b(:,:)
  real(8) :: tr
  tr = 0
  do i = 1, size(a,2)
   do j = 1, size(b,1)
    tr = tr + a(i,j) * b(j,i)
   enddo
  enddo
 end function func_tr_seki
end module subprogs

program main
 use subprogs
 implicit none
 real(8), allocatable :: a(:,:),b(:,:)
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

 write(*,*) 'tr(A) =' , func_tr(a)
 write(*,*) 'tr(B) =' , func_tr(b)

 write(*,*) 'tr(AB) =' , func_tr_seki(a,b)
 write(*,*) 'tr(BA) =' , func_tr_seki(b,a)

if(func_tr_seki(a,b) == func_tr_seki(b,a)) then
 write(*,*) 'tr(AB) = tr(BA)'
else
 write(*,*) 'tr(AB) = tr(BA)'
endif 
end program main
   
