program p2_8_2
 implicit none
 real(8) a(3),b(3),c(3),l,m,n,s,ans
 write(*,*) 'Input P(x,y,z)'
 read(*,*) a(1:3)
 write(*,*) 'Input Q(x,y,z)'
 read(*,*) b(1:3)
 c(1:3)=b(1:3)-a(1:3)
 write(*,*) 'c(1:3)=',c(1:3)
 l=sqrt(a(1)**2+a(2)**2+a(3)**2)
 m=sqrt(b(1)**2+b(2)**2+b(3)**2)
 n=sqrt(c(1)**2+c(2)**2+c(3)**2)
 s=(l+m+n)/2
 ans=sqrt(s*(s-l)*(s-m)*(s-n))
 write(*,*) 'A=',ans
end program p2_8_2