module subprogs
  implicit none
contains
subroutine sor(phi, omg, x, n1, n2, c, d, er, er0, itrmax)
  integer, intent(in) :: n1, n2, itrmax
  real(8), intent(in) :: c, d, x, er0, omg
  real(8), intent(inout) :: phi(n1, n2)
  real(8) er, rhs
  integer i, j, itr
 call set_dbc(phi, x, n1, n2)
 do itr = 1 ,itrmax
  do j = 2, n2-1
    do i = 2, n1-1
      rhs = -c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-1) + phi(i, j+1))
      phi(i, j) = phi(i, j) + omg * (rhs - phi(i, j))
    end do
  end do
  call chk_err(phi, c, d, n1, n2, er)
  write(*,*) ' itr, er = ', itr, er
  if ( er < er0) exit
end do
end subroutine sor

subroutine chk_err(phi, c, d, n1, n2, er)
  real(8), intent(in) :: c, d, phi(n1,n2)
  integer, intent(in) :: n1, n2
  real(8) er, rhs
  integer i, j
  er  = 0.0d0
  rhs = 0.0d0
  do j = 2, n2-1
    do i = 2, n1-1
      rhs = -c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-1) + phi(i, j+1))
      er = er + (rhs - phi(i, j)) ** 2
    end do
  end do
end subroutine chk_err


subroutine set_dbc(phi, x, n1, n2)
  integer, intent(in) :: n1 ,n2
  integer i, j
  real(8), intent(in) :: x
  real(8), intent(out) :: phi(n1, n2)
  real(8) pi, c, d
  pi = 2.0 * acos(0.0d0)
  phi(:, :) = 0.0d0
  do j = 1, n2
   do i = 1, n1
    if ( j == 1) then
     phi(i, j) = sin(pi * x * (i-1))
    else if (j == n2) then
     phi(i, j) = 0.0d0
    else if (i == 1) then
     phi(i, j) = 0.0d0
    else if (i == n1) then
     phi(i, j) = 0.0d0
    end if
   end do
  end do
end subroutine set_dbc

function parameter(n) result(omg)
 integer, intent(in) :: n
 real(8) omg, pi
 pi = 2.0d0 * acos(0.0d0)
 omg = 2 / (1 + sin(pi / (n - 1)))
end function parameter

subroutine logic(phi_logic, x, n1, n2)
  real(8), intent(in) :: x
  integer, intent(in) :: n1, n2
  real(8) phi_logic(n1, n2)
  real(8) pi
  integer i, j
  phi_logic(:, :) = 0.0d0
  pi = 2.0d0 * acos(0.0d0)
  do j = 1, n2
    do i = 1, n1
      phi_logic(i, j) = sin(pi * x * (i-1)) * sinh(pi * (1- x * (j-1))) / sinh(pi)
    end do
  end do
end subroutine logic
end module subprogs


program ensyu18
  use subprogs
  implicit none
  real(8), allocatable :: phi(:, :), phi_logic(:, :)
  real(8) :: omg, x, er0 = 1.0d-6, c, d, er
  integer :: n1, n2, i, j, k, itrmax = 100
  character (len=3) :: fno, fno2
  character (len=40) :: fname, fname2

  n1 = 20
  n2 = 20
  x = 1.0d0 / (n1-1)
  c = - x**2 / (2 * (x**2 + x**2))
  d = (x/x) ** 2 * c
  omg = parameter(n1)
  allocate(phi(n1, n2), phi_logic(n1, n2))
  call sor(phi, omg, x, n1, n2, c, d, er, er0, itrmax)
  do j  = 1, n2
    do i = 1, n1
      write(*,*) phi(i, j)
    end do
  end do
  !x1は固定して、x2ごとのφの大きさを出力する
  do i = 1, n1
    write(fno, fmt = '(i3.3)') i
    fname = 'x1_1'//fno//'.d'
    do j = 1, n2
      open(i, file = 'x1_1'//fno//'.d')
      write(i,*) x * (j - 1), phi(i, j)
  end do
    close(i)
 end do
  call logic(phi_logic, x, n1, n2)
  do i = 1, n1
    write(fno2, fmt = '(i3.3)') i
    fname2 = 'x1_2'//fno//'.d'
    do j = 1, n2
      open(i, file = 'x1_2'//fno2//'.d')
      write(i,*) x * (j - 1), phi_logic(i, j)
    end do
    close(i)
  end do
!ｘ２を固定する
  do j = 1, n2
    write(fno, fmt = '(i3.3)') j
    fname = 'x2_1'//fno//'.d'
    do i = 1, n1
      open(j, file = 'x2_1'//fno//'.d')
      write(j,*) x * (i - 1), phi(i, j)
  end do
    close(j)
 end do
  do j = 1, n2
    write(fno2, fmt = '(i3.3)') j
    fname2 = 'x2_2'//fno//'.d'
    do i = 1, n2
      open(j, file = 'x2_2'//fno2//'.d')
      write(j,*) x * (i - 1), phi_logic(i, j)
    end do
    close(j)
  end do
end program ensyu18
