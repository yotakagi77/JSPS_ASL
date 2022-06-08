program module
    implicit none
    integer a, p, k
    write(*,*) 'input natural number a'
    read(*,*) a 
    write(*,*) 'input natural number p'
    read(*,*) p 
    k = a - p * int(a / p)
    write(*,*) 'mod(a,p) = ', k 
end program module