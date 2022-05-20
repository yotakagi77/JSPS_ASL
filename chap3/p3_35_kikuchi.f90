module eval_2x2mat
    implicit none
    contains
    function eval2x2mat(a) result(eval)
        real(8), intent(in) :: a(:,:)
        complex(8) eval(2)
        real(8) b, c, d, e
        if (size(a, 1) /= size(a, 2)) stop 'not square'
        if (size(a, 1) /= 2) stop 'not 2x2 matrix'
        b = - 0.5d0 * (a(1,1) + a(2,2))
        c = a(1,1) * a(2,2) - a(1,2) * a(2,1)
        d = (b**2) - c
        if (d < 0.0d0) then
            eval(1) = cmplx(-b, sqrt(-d), kind = 8)
            eval(2) = conjg(eval(1))
        else if (d > 0.0d0) then
            e = -b + sign(sqrt(d), -b)
            eval(1) = cmplx(e    , 0.0d0, kind = 8)
            eval(2) = cmplx(c / e, 0.0d0, kind = 8)
        else
            eval(1) = cmplx(-b, 0.0d0, kind = 8)
            eval(2) = eval(1)
        end if
    end function eval2x2mat
end module eval_2x2mat


program eval_22mat
    use eval_2x2mat
    implicit none
    integer i
    real(8) a(2,2)
    complex(8) eval(2)

    call random_seed
    call random_number(a)

    write(*,*) "a ="
    do i = 1, 2
        write(*,*) a(i,:)
    end do

    eval(:) = eval2x2mat(a)
    write(*,*) 'eval_1 =', eval(1)
    write(*,*) 'eval_2 =', eval(2)

end program eval_22mat