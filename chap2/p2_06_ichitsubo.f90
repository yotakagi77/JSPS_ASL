program name
    implicit none
    integer i
    real(kind(1d0)) u(3),cos_alpha,cos_beta,cos_gamma,norm,base(3,3)

    !call random_number(u)
    write(*,*)"input u1 :"
    read(*,*) u(1)
    write(*,*)"input u2 :"
    read(*,*) u(2)
    write(*,*)"input u3 :"
    read(*,*) u(3)
    base=0.0d0
    do i=1,3
        base(i,i)=1.0d0
    end do
    
    norm=sqrt(dot_product(u,u))

if (norm==0.0d0) stop "stop"

cos_alpha=dot_product(base(1,:),u)/norm
cos_beta=dot_product(base(2,:),u)/norm
cos_gamma=dot_product(base(3,:),u)/norm
write(*,*) "u(i)=",u(:)
write(*,*) "cos_alpha=",cos_alpha
write(*,*) "cos_beta=",cos_beta
write(*,*) "cos_gamma=",cos_gamma
write(*,*) "cos_alpha^2+cos_beta^2+cos_gamma^2=",cos_alpha**2+cos_beta**2+cos_gamma**2
end program name
