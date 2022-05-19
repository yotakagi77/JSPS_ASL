program name
    implicit none
    integer n,m,i,j,im
    real(kind(1d0)) x,L,lambda,T,k,time,phi(101),x1(101),dt,dx,phi_prev(101)
    character::filename*40,filename2*40
    

    L=1.0d0
    k=0.01
    m=100
    do n=0,10
        time=dble(n)
        T=0.0d0
        write(filename,'("./output6_21_"i2.2,".dat")') n
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


    im=101
    dx=L/dble(im-1)

    do i=1,im
        x1(i)=dble(i-1)*L/dble(im-1)
        if (x1(i) <=L/2.0d0) then
            phi(i)=x1(i)
        else 
            phi(i)=L-x1(i)
        end if
    end do
    dt =0.0010d0
    t=0
do
!!!!!!!時間積分!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        phi_prev=phi
    do i=2,im-1
        phi(i)=k/dx/dx*(phi(i+1)-2.0d0*phi(i)+phi_prev(i-1))*dt+phi(i)
    end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if (mod(int(t/dt),1000)==0) then
        write(filename2,'("./output6_21_calc_"i2.2,".dat")') int(t)
        open(20,file=filename2,status='replace')
        do i=1,im
            write(20,*) x1(i),phi(i)
        end do
    end if


    if (int(t)>=10) then
        exit
    end if

    t=t+dt

end do


    


end program name