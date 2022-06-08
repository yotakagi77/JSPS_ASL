program name
    implicit none
    integer n,m,i,j
    real(kind(1d0)) x,L,lambda,T,k,time
    character::filename*40
    

    L=1.0d0
    k=0.01
    m=100
    do n=0,10
        time=dble(n)
        T=0.0d0
        write(filename,'("./output1_24_5_"i2.2,".dat")') n
        open(17,file=filename,status='replace')
        
        do j=1,m+1
            x=L/dble(m)*dble(j-1)
            T=0.0d0
                do i=1,50
                    lambda=k*(dble(2*i-1)*acos(-1d0)/L)**2.0d0
                    T=T+ 4.0d0*L/acos(-1d0)/acos(-1d0) *exp(-lambda*time)*sin(dble(2*i-1)*acos(-1d0)*x/L)*((-1.0d0)**dble(i-1))&
                    /(dble(2*i-1)**2.0d0)
                end do
        write(17,*) x,T
        end do
        
    end do
end program name
