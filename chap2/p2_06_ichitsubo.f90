program name
    implicit none
    integer i
    real(kind(1d0)) u(3),cos_alpha,cos_beta,cos_gamma,norm

    !call random_number(u)
    write(*,*)"input u1 :"
    read(*,*) u(1)
    write(*,*)"input u2 :"
    read(*,*) u(2)
    write(*,*)"input u3 :"
    read(*,*) u(3)
    
    norm=0
do i=1,3
    norm=norm+u(i)*u(i)
end do

if (norm==0.0d0) stop "stop"
norm=Sqrt(norm)
cos_alpha=u(1)/norm
cos_beta=u(2)/norm
cos_gamma=u(3)/norm
write(*,*) "u(i)=",u(:)
write(*,*) "cos_alpha=",cos_alpha
write(*,*) "cos_beta=",cos_beta
write(*,*) "cos_gamma=",cos_gamma
write(*,*) "cos_alpha^2+cos_beta^2+cos_gamma^2=",cos_alpha**2+cos_beta**2+cos_gamma**2
end program name
