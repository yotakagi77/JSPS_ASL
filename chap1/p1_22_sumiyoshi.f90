program equation

implicit none
real:: a, b, c, D, x, x1, x2, e, f

write(*,'(a,$)') 'input a'
read(*,*) a

write(*,'(a,$)') 'input b'
read(*,*) b

write(*,'(a,$)') 'input c'
read(*,*) c

D = b**2 - 4*a*c
write(*,*) 'D=',D

if(D >= 0) then
 x1 = -b/(2*a) + sqrt(D)/(2*a)
 x2 = -b/(2*a) - sqrt(D)/(2*a)
 write(*,*)'x1=', x1, 'x2=', x2

else
 e = -b/(2*a)
 f = sqrt(-D)/(2*a)
 write(*,*) e,'+',f,'i'
endif

end  program equation