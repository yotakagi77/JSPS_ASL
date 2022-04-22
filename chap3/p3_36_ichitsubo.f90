module subprog
    implicit none
contains
    function eval2x2mat(a) result(eval)
        real(kind(1d0)) ,intent(in):: a(:,:)
        complex(kind(1d0)) eval(2)
        real(kind(1d0)) b,c,d,e
        if(size(a,1)/=size(a,2)) stop "not square"
        if (size(a,1)/=2) stop "not 2x2 matrix"
        b = -0.5d0*(a(1,1)+a(2,2))
        c = a(1,1)*a(2,2)-a(1,2)*a(2,1)
        d = b**2-c
        if (d<0.0d0) then
            eval(1)=cmplx(-b,sqrt(-d),kind(1d0))
            eval(2)=conjg(eval(1))
        else if (d>0.0d0) then
            e=-b+sign(sqrt(d),-b)
            eval(1)=cmplx(e,0.0d0,kind(1d0))
            eval(2)=cmplx(c/e,0.0d0,kind(1d0))
        else
            eval(1)=cmplx(-b,0.0d0,kind(1d0))
            eval(2)=eval(1)
        end if
    end function eval2x2mat

    function vec(p) result(v)
        complex(kind(1d0)) p(2,2),v(2)
        v(1)=-p(1,2)/sqrt((p(1,2)*p(1,2)+p(1,1)*p(1,1)))
        v(2)=p(1,1)/sqrt((p(1,2)*p(1,2)+p(1,1)*p(1,1)))
    end function vec
end module subprog

program name
    use subprog
    implicit none
    integer i
    real(kind(1d0)) a(2,2)
    complex(kind(1d0)) v(2),eval(2),p(2,2)

    call random_seed
    call random_number(a)

    write(*,*) "a="
    do i=1,2
        write(*,*) a(i,:)
    end do

    eval = eval2x2mat(a)
    
    do i=1,2
        p=a
        p(1,1)=a(1,1)-eval(i)
        p(2,2)=a(2,2)-eval(i)
        v=vec(p)
        write(*,*) "eval="
        write(*,*) eval(i)
        write(*,*) "evec="
        write(*,*) v
        write(*,*) "(A-LI)v="
        write(*,*) matmul(p,v)
    end do


    
end program name