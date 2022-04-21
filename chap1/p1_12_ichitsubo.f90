program ensyu
implicit none
integer i
real(kind(1d0)) a,r,sum
a=16.0d0
r=0.8d0
sum=0.0d0
do i=1,10

    write(*,*) "n=",i,"","an=",a*r**dble(i-1)
    sum=sum+a*r**dble(i-1)

end do

write(*,*) "LOOP:",sum,"FORMULA:",a*(1-r**10)/(1-r)

end program ensyu

