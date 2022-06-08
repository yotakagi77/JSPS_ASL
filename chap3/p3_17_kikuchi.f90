module rotation
    implicit none
contains
    subroutine rot(x, y, sheta)
        real(8), intent(in) :: x(2), sheta
        real(8), intent(out) :: y(2)
        real(8) pi, rad
        pi = 2.d0 * acos(0.d0)
        rad = (sheta / 1.8d2) * pi
        y(1) = x(1) * cos(rad) - x(2) * sin(rad)
        y(2) = x(1) * sin(rad) + x(2) * cos(rad)
    end subroutine rot
end module rotation

program rotatex
    use rotation
    implicit none
    real(8) x(2), y(2), sheta
    write(*,*) 'input vector x(_, )'
    read(*,*) x(1)
    write(*,*) 'input vector x( ,_)'
    read(*,*) x(2)
    write(*,*) 'input sheta[degree]'
    read(*,*) sheta
    call rot(x, y, sheta)
    write(*,*) y
end program rotatex
