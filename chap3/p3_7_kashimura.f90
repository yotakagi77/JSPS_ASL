!p84 演習3.7
module subprogs      
  implicit none
contains
  subroutine cuboid_vol(l,b,h,v)
    double precision,intent(in) :: l,b,h
	double precision,intent(out) :: v
	double precision :: s,pi
	v=l*b*h
  end subroutine cuboid_vol
end module subprogs

program main
  use subprogs
  implicit none
    double precision :: l,b,h,v
    write(*,*) "Input dimensions of cuboid 'l,b,h' "
    read(*,*) l,b,h
    call cuboid_vol(l,b,h,v)
    write(*,*) 'v=',v
end program main