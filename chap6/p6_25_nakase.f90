module globals
  integer, save :: i, j
  real(8), save :: h0 = 1.0d0, xl = 1.0d2, gr=9.8d0, dt = 0.1d0
  character (len=3) :: fno
  character (len=30) :: fname
end module globals

subroutine output(eta, x, n1, n2, itr)
  use globals
  implicit none
  real(8), intent(in) :: eta(n1, n2, 3)
  real(8), intent(inout) :: x(2, n1, n2)
  integer, intent(in) :: n1, n2, itr
  !計算ステップごとに出力ファイルを作成
  write(fno, fmt='(i3.3)') itr
  fname = 'output_6.25'//fno//'.d'
    open(itr, file='output6.25_'//fno//'.d')
    do i = 1, n1
      do j = 1, n2
        write(itr, *) x(1:2, i, j), eta(i, j, 3)
      end do
    end do
  close(itr)
end subroutine

subroutine set_int(x, eta, n1, n2, dx, dh)
  use globals
  implicit none
  real(8), intent(in) :: dx(2), dh
  real(8), intent(inout) :: x(2, n1, n2), eta(n1, n2, 3)
  integer, intent(in) :: n1, n2
  do j = 1, n2
    do i = 1, n1
      x(1, i, j) = dx(1) * dble(i-1)
      x(2, i, j) = dx(2) * dble(j-1)
    end do
  end do
  do i = 1, n1
    do j = 1, n2
      eta(i, j, 1) = h0 + dh * exp(-((x(1, i, j)-0.5d0*xl)**2 + (x(2, i, j)-0.5d0*xl)**2) / 1.0d2)
    end do
  end do
  eta(:, :, 2) = eta(:, :, 1)
end subroutine set_int

program ensyu25
  use globals
  implicit none
  real(8) c(2), c0, dh, fr, dx(2)
  real(8), allocatable :: eta(:, :, :), x(:, :, :)
  integer :: n1, n2, itr, itrmax = 100
  write(*,*) 'input n1, n2, fr'
  read(*,*) n1, n2, fr
  dx(1) = xl / dble(n1-1)
  dx(2) = xl / dble(n2-1)
  dh = 0.1d0 * h0
  allocate(eta(n1, n2, 3), x(2, n1, n2))
  call set_int(x, eta, n1, n2, dx, dh) !波形の初期値を設定
  open(10, file='output0.d')
  do i = 1, n1
    do j = 1, n2
      write(10, *) x(1:2, i, j), eta(i, j, 1:2)
    end do
  end do
  close(10)
  c(1:2) = gr * h0 * dt **2 / dx(1:2) **2
  c0 = 2.0d0 * (1.0d0 - c(1) - c(2))
  do itr = 1, itrmax
    do j = 2, n2-1
      do i = 2, n1-1
        eta(i, j, 3) = c0*eta(i,j,2) - eta(i,j,1)+c(1)*(eta(i-1,j,2)+eta(i+1,j,2)) + c(2)*(eta(i,j-1,2)+eta(i,j+1,2))
      end do
    end do
    if (mod(itr, 10) == 0) call output(eta, x, n1, n2, itr) !時間ごとに出力
    eta(2:n1-1, 2:n2-1, 1) = eta(2:n1-1, 2:n1-1, 2)
    eta(2:n1-1, 2:n2-1, 2) = eta(2:n1-1, 2:n2-1, 3)
    eta(1, :, :) = eta(2, : ,:)
    eta(n1, :, :) = eta(n1-1, :, :)
    eta(:, 1, :) = eta(:, 2, :)
    eta(:, n2, :) = eta(:, n2-1, :)
  end do
end program ensyu25
