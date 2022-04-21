program ensyu
implicit none
integer m,n,a,b,sum1,sum2

write(*,*) "input m (input 0 to stop):"
read(*,*) m
write(*,*) "input n (input 0 to stop):"
read(*,*) n

if (n /= 0 .and. m /=0) then
a=max(m,n)
b=min(m,n)

sum1=a*(a+1)/2
sum2=(b-1)*b/2

write(*,*) sum1-sum2

end if


end program ensyu