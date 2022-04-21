program ensyu
    implicit none
    integer i
    real(kind(1d0)) a,r,sum
    a=16.0d0
    r=0.8d0
    sum=0.0d0
    open(17, file='output1_18.d', status='replace')
    do i=1,30
    
        write(17,*) i,a*r**dble(i-1)
    
    end do
    
    
    end program ensyu