module subprog
    implicit none
contains
   function stp(u,v,w) result(ans)
    real(kind(1d0))  u(3),v(3),w(3), a, b, c, ans

    a = w(1) * (u(2) * v(3) - u(3) * v(2))
    b = w(2) * (u(3) * v(1) - u(1) * v(3))
    c = w(3) * (u(1) * v(2) - u(2) * v(1))
    ans = a + b + c
   
   end function stp

end module subprog

program main
use subprog
implicit none
 real(kind(1d0)) a(3), b(3), c(3)
 call random_seed
 call random_number(a)
 call random_number(b)
 call random_number(c)
 write(*,*) 'Scalar triple product = ', stp(a,b,c)
end program


