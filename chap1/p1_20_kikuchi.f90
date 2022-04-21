program circle
    implicit none
    real(8) d, x, y, pi, r, theta
    integer :: n, i, fo = 10
    pi = 2.0d0 * acos(0.0d0)
    r = 10
    n = 100
    open(fo, file = 'output_circle.d')
    d = 2 * pi / dble(n)
    do i = 1, n
        theta = d * i 
        x = r * cos(theta)
        y = r * sin(theta)
        write(fo,*) x, '', y
    end do
    close(fo)
end program circle