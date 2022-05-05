program heron
    implicit none
    real(8) p(3), q(3), a, b, c, s, h, cp
    write(*,*) 'input vector op'
    write(*,*) 'p_x : '
    read(*,*) p(1)
    write(*,*) 'p_y : '
    read(*,*) p(2)
    write(*,*) 'p_z : '
    read(*,*) p(3)
    write(*,*) 'input vector oq'
    write(*,*) 'q_x : '
    read(*,*) q(1)
    write(*,*) 'q_y : '
    read(*,*) q(2)
    write(*,*) 'q_z : '
    read(*,*) q(3)
    cp = 0.5d0 * sqrt(((p(2)*q(3) - p(3)*q(2))**2) + ((p(3)*q(1) - p(1)*q(3))**2) + ((p(1)*q(2) - p(2)*q(1))**2))
    write(*,*) 'A(cross product) = ', cp
    a = sqrt((p(1)**2) + (p(2)**2) + (p(3)**2))
    b = sqrt((q(1)**2) + (q(2)**2) + (q(3)**2))
    c = sqrt(((q(1) - p(1))**2) + ((q(2) - p(2))**2) + ((q(3) - p(3))**2))
    s = (a + b + c) / 2
    h = sqrt(s * (s - a) * (s - b) * (s - c))
    write(*,*) 'A(heron) = ', h
end program heron