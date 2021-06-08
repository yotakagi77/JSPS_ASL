module mat_subprogs
 implicit none
contains
 subroutine print_rmatc(a, gyouretu)
 character(*), intent(in) :: gyouretu
 real(8), intent(in) :: a(: , :)
 integer i, n, m
!形状引継ぎ配列だから、1次元目と2次元目の寸法を出す。
 n = size(a, 1)
 m = size(a, 2)
 write(*,*) gyouretu
 do i = 1, n
  write(*, '(100e12.4)') a(i, 1:m)
 end do
 end subroutine print_rmatc

 subroutine print_imatc(ia, gyouretu2)
 character(*), intent(in) :: gyouretu2
 integer, intent(in) :: ia(:, :)
 integer i, n, m
 n = size(ia, 1)
 m = size(ia, 2)
 write(*,*) gyouretu2
 do i = 1, n
  write(*, '(5i6)') ia(i, 1:m)
 end do
 end subroutine print_imatc
end module mat_subprogs


program ensyu27
 use mat_subprogs
 implicit none
 real(8) b(2,3)
 integer ib(2, 3)
 write(*,*) 'input b'
 read(*,*) b
 write(*,*) 'input ib'
 read(*,*) ib
 call print_rmatc(b, 'matrix b')
 call print_imatc(ib, 'matrix ib')
end program ensyu27