module geometric_progression
    implicit none
    contains
    function geoprog(a, r, n) result(sum)
        real(8), intent(in) :: a, r
        real(8) sum
        integer, intent(in) :: n
        integer i
        sum = 0.d0
        do i = 1, n
            sum = sum + a * (r**dble(i-1))
        end do
    end function geoprog
end module geometric_progression


program gp
    use geometric_progression
    implicit none
    real(8) a, r
    integer n
    write(*,*) 'input a :'
    read(*,*) a
    write(*,*) 'input r :'
    read(*,*) r
    write(*,*) 'input n :' 
    read(*,*) n 
    write(*,*) 'sum = ', geoprog(a, r, n)
end program gp