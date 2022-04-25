program Prime_factorization

implicit none

integer :: n, i, j, k,l
integer, dimension(10000) :: a 


do


write(*, *) 'please input n (if n <= 1 STOP) '
read(*, *) n
if (n <= 1 ) stop ' Please input the number that is bigger than 1 ! '
if (n > 10000) stop ' Is it bigger than 10,000 ! '

i = 2
k = 1

do
if (i <= int(sqrt(dble(n))) ) then

if (mod(n, i) == 0) then
n = n / i
a(k) = i 
k = k+1


cycle

else
i = i + 1
cycle
endif

else
exit
endif

enddo
a(k) = n


do l = 1, k
 if(l < k) then 
  write(*,fmt='(I5)',advance = 'no') a(l)
  write(*,fmt='(a2)',advance = 'no') '*'
 else
  write(*,fmt='(I5)') a(l)
 endif
enddo 
enddo
end program Prime_factorization       
                