module subprogs
    implicit none
contains
   function det_1(a,b) result(dx)
    real(kind(1d0)) a(3),b(3),c(2,3),dummy(2,3),dx
    
    c(1,:)=a(:)
    c(2,:)=b(:)

    dummy=cshift(c,1,2)
        dx =dummy(1,1)*dummy(2,2)-dummy(1,2)*dummy(2,1)
   end function det_1

   function det_2(a,b) result(dy)
    real(kind(1d0)) a(3),b(3),c(2,3),dummy(2,3),dy
    
    c(1,:)=a(:)
    c(2,:)=b(:)

    dummy=cshift(c,2,2)
        dy =dummy(1,1)*dummy(2,2)-dummy(1,2)*dummy(2,1)
   end function det_2

   function det_3(a,b) result(dz)
    real(kind(1d0)) a(3),b(3),c(2,3),dummy(2,3),dz
    
    c(1,:)=a(:)
    c(2,:)=b(:)

    dummy=cshift(c,3,2)
        dz =dummy(1,1)*dummy(2,2)-dummy(1,2)*dummy(2,1)
   end function det_3

   function dot_1(u,v) result(a)
    real(kind(1d0))  u(3),v(3), a

    a = u(2) * v(3) - u(3) * v(2)
   end function dot_1

   function dot_2(u,v) result(b)
    real(kind(1d0))  u(3),v(3), b

    b = u(3) * v(1) - u(1) * v(3)
   end function dot_2

   function dot_3(u,v) result(c)
    real(kind(1d0))  u(3),v(3), c

    c = u(1) * v(2) - u(2) * v(1)
   end function dot_3

end module subprogs

program main
use subprogs
implicit none
 real(kind(1d0)) d(3),e(3)
 call random_seed
 call random_number(d)
 call random_number(e)
  write(*,*) 'enshu3-21'
  write(*,*) "i1:"
  write(*,*) det_1(d,e)
  write(*,*) "i2:"
  write(*,*) det_2(d,e)
  write(*,*) "i3:"
  write(*,*) det_3(d,e)

  write(*,*) 'enshu2-07'
  write(*,*) "i1:"
  write(*,*) dot_1(d,e)
  write(*,*) "i2:"
  write(*,*) dot_2(d,e)
  write(*,*) "i3:"
  write(*,*) dot_3(d,e)


end program main
