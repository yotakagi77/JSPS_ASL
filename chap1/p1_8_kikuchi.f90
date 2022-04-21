program prime_num
    implicit none
    integer i, n, k 
    write(*,*) 'input natulal number n (<=1,000,000)'
    read(*,*) n 
    k = int(sqrt(dble(n)))
    if (n < 1) stop 'stop, n < 1'
    if (n > 1000000) stop 'stop, n > 1,000,000'
    do i = 2, k 
        if (mod(n, i) == 0) then
            write(*,*) 'n is not a prime number'
                exit
        else if (i == k) then
            write(*,*) 'n is a prime number !'
        end if
    end do
end program prime_num
