program ensyu
implicit none
integer i,m,n,a,b

write(*,*) "input m :"
read(*,*) m

write(*,*) "input n :"
read(*,*) n

if (m>10001 .or. m<1) stop "stop"
if (n>10001 .or. n<1) stop "stop"

a=max(m,n)
b=min(m,n)

do 
i=b
b=mod(a,b)
a=i
if(b==0) then
write(*,*) "The gcd of m and n:",a
exit
end if

end do

end program ensyu