module subprog
implicit none
contains
 function det_3(a) result(det)
  real(8) :: a(3,3), det, x1, x2, x3, x4, x5, x6
  x1 = a(1,1) * a(2,2) * a(3,3)  
  x2 = a(1,2) * a(2,3) * a(3,1)
  x3 = a(1,3) * a(2,1) * a(3,2)
  x4 = a(1,1) * a(2,3) * a(3,2)
  x5 = a(1,2) * a(2,1) * a(3,3)
  x6 = a(1,3) * a(2,2) * a(3,1)
  det = x1 + x2 + x3 - x4 - x5 - x6
 
 end function det_3
end module subprog

program main
 use subprog
 implicit none
 integer i, j, n, k, l, q, r, m
 real(8) :: a(3,3), b(3,3), c(3,3), d(3,3), s, t
 call random_number(a(1:3, 1:3))
 write(*,*) 'input the row you want to change'
 read(*,*) n
 write(*,*) 'how many times do you want to double? input the number.'
 read(*,*) s
 if(n == 1) then
  do i = 1,3
   b(1,i) = s * a(1,i)
   b(2,i) = a(2,i)
   b(3,i) = a(3,i)
  enddo
 endif
 if(n == 2) then
  do i = 1,3
   b(1,i) = a(1,i)
   b(2,i) = s * a(2,i)
   b(3,i) = a(3,i)
  enddo
 endif
 if(n == 3) then
  do i = 1,3
   b(1,i) = a(1,i)
   b(2,i) = a(2,i)
   b(3,i) = s * a(3,i)
  enddo
 endif

 write(*,*) det_3(a)
 write(*,*) det_3(b)

 write(*,*) 'input the row number(0<k<3) you want to replace'
 read(*,*) k
 write(*,*) 'input the row number(l : k<l) you want to replace'
 read(*,*) l

 if(k == 1 .and. l == 2) then
  c(1,1:3) = a(2,1:3)
  c(2,1:3) = a(1,1:3)
  c(3,1:3) = a(3,1:3)
 endif
 if(k == 1 .and. l == 3) then
  c(1,1:3) = a(3,1:3)
  c(2,1:3) = a(2,1:3)
  c(3,1:3) = a(1,1:3)
 endif
 if(k == 2 .and. l == 3) then
  c(1,1:3) = a(1,1:3)
  c(2,1:3) = a(3,1:3)
  c(3,1:3) = a(2,1:3)
 endif

 write(*,*) det_3(a)
 write(*,*) det_3(c)

 write(*,*) 'input the row you want to change'
 read(*,*) q
 write(*,*) 'how many times do you want to double? input the number.'
 read(*,*) t
 write(*,*) 'which row would you like to add? input the number'
 read(*,*) r 

 if(q == 1 .and. r == 2) then
  do m = 1,3
   d(1,i) = a(1,i)
   d(2,i) = t * a(1,i) + a(2,i)
   d(3,i) = a(3,i)
  enddo
 endif
 if(q == 1 .and. r == 3) then
  do m = 1,3
   d(1,m) = a(1,m)
   d(2,m) = a(2,m)
   d(3,m) = t * a(1,m) + a(3,m)
  enddo
 endif
 if(q == 2 .and. r == 3) then
  do m = 1,3 
   d(1,m) = a(1,m)
   d(2,m) = a(2,m)
   d(3,m) = t * a(2,m) + a(3,m)
  enddo
 endif    
 if(q == 1 .and. r == 2) then
  do m = 1,3
   d(1,m) = a(1,m)
   d(2,m) = t * a(1,m) + a(2,m)
   d(3,m) = a(3,m)
  enddo
 endif
 if(q == 2 .and. r == 1) then
  do m = 1,3
   d(1,m) = a(1,m) + t * a(2,m)
   d(2,m) = a(2,m)
   d(3,m) = a(3,m)
  enddo
 endif
 if(q == 3 .and. r == 2) then
  do m = 1,3
   d(1,m) = a(1,m)
   d(2,m) = a(2,m) + t * a(3,m)
   d(3,m) = a(3,m)
  enddo
 endif
 if(q == 3 .and. r == 1) then
  do m = 1,3
   d(1,m) = a(1,m) + t * a(3,m)
   d(2,m) = a(2,m)
   d(3,m) = a(3,m)
  enddo
 endif

 write(*,*) det_3(a)
 write(*,*) det_3(d)

end program main
 