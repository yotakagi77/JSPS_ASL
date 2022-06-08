program judge
    use interface_mod
    implicit none
    real(8),allocatable :: a(:,:)
    integer i, j, n
    write(*,*) "input n :"
    read(*,*) n
    allocate(a(n,n))
    read(*,*) a
     do i = 1, n
      write(*,*) (a(i,j), j = 1,n)
     enddo
     write(*,*) chk_dd_mat(n,a)
end program judge