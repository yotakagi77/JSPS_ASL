module file_mat
 implicit none
 interface output_mat
  subroutine print_imat(fn0, ai, n, chara)
   character(*), intent(in) :: chara
   integer, intent(in) ::  fn0, n
   integer i
   integer, intent(out) :: ai(n, n)
  end subroutine print_imat

  subroutine print_imat2(fn0, ai2, chara)
   character(*), intent(in) :: chara
   integer, intent(in) ::  fn0
   integer i, n1, n2
   integer, intent(out) :: ai2(:, :)
  end subroutine print_imat2


  subroutine print_mat(fn0, a, n, chara)
   character(*), intent(in) :: chara
   integer, intent(in) ::  fn0, n
   real(8), intent(out) :: a(n, n)
   integer i
  end subroutine print_mat

  subroutine print_mat2(fn0, a2, chara)
   character(*), intent(in) :: chara
   integer, intent(in) ::  fn0
   real(8), intent(out) :: a2(:, :)
   integer i, n1, n2
  end subroutine print_mat2

 end interface
end module file_mat


!リスト3.16整数行列を出力する。形状明示
 subroutine print_imat(fn0, ai, n, chara)
  character(*), intent(in) :: chara
  integer, intent(in) ::  fn0, n
  integer i
  integer, intent(out) :: ai(n, n)
  read(fn0, *) ai(1:n, 1:n)
  write(*,*) chara
  do i = 1, n
   write(*,*) ai(i, 1:n)
  end do
 end subroutine print_imat

!リスト3.16整数行列を出力する。形状引継ぎ
 subroutine print_imat2(fn0, ai2, chara)
  character(*), intent(in) :: chara
  integer, intent(in) ::  fn0
  integer i, n1, n2
  integer, intent(out) :: ai2(:, :)
  n1 = size(ai2, 1)
  n2 = size(ai2, 2)
  read(fn0, *) ai2(1:n1, 1:n2)
  write(*,*) chara
  do i = 1, n1
   write(*,*) ai2(i, 1:n2)
  end do
 end subroutine print_imat2

!リスト3.7倍制度実数型。形状明示。
 subroutine print_mat(fn0, a, n, chara)
  character(*), intent(in) :: chara
  integer, intent(in) ::  fn0, n
  real(8), intent(out) :: a(n, n)
  integer i
  read(fn0, *) a(1:n, 1:n)
  write(*,*) chara
  do i =1, n
   write(*, *) a(i, 1:n)
  end do
 end subroutine print_mat


!リスト3.7倍制度実数型。形状引継ぎ
 subroutine print_mat2(fn0, a2, chara)
  character(*), intent(in) :: chara
  integer, intent(in) ::  fn0
  real(8), intent(out) :: a2(:, :)
  integer i, n1, n2
  n1 = size(a2, 1)
  n2 = size(a2, 2)
  read(fn0, *) a2(1:n1, 1:n2)
  write(*,*) chara
  do i =1, n1
   write(*, *) a2(i, 1:n2)
  end do
 end subroutine print_mat2

program ensyu5
 use file_mat
 implicit none
 real(8), allocatable :: a(:, :)
 real(8) a2(2, 2)
 integer, allocatable :: ai(:, :)
 integer ai2(2, 2)
 integer :: fn0 = 10, n
 open(fn0, file='input5.txt')
 read(fn0, *) n
 allocate(a(1:n, 1:n), ai(1:n, 1:n))
 call output_mat(fn0, a, n, 'matrix A is ')
 call output_mat(fn0, ai, n, 'matrix Ai is ')
 call output_mat(fn0, a2, 'matrix A2 is ')
 call output_mat(fn0, ai2, 'matrix Ai2 is ')
end program ensyu5