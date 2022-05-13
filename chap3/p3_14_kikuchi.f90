module statistics
    implicit none
contains
    subroutine stat(ia, sum, ave, dis, dev, n)
        real(8), intent(out) :: sum, ave, dis, dev
        real(8), intent(in) :: ia(:)
        integer n, i
        sum = 0
        do i = 1, n
            sum = sum + ia(i) 
        end do
        ave = sum / dble(n)
        do i = 1, n
            dis = dis + (ia(i) - ave) **2
        end do
        dis = dis / dble(n)
        dev = sqrt(dis)
    end subroutine stat    
end module statistics


program sum_ave_dis_dev
    use statistics
    real(8) sum, ave, dis, dev
    real(8), allocatable :: a(:)
    integer n

    write(*,*) 'input n(>=2)'
    read(*,*) n 
    if (n < 2) stop 'stop n < 2'
    allocate (a(n))
    call random_seed
    call random_number(a(n))

    call stat(a, sum, ave, dis, dev, n)
    write(*,*) 'sum = ', sum
    write(*,*) 'ave = ', ave
    write(*,*) 'dis = ', dis
    write(*,*) 'dev = ', dev
end program sum_ave_dis_dev