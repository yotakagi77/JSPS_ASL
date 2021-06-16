module globals
  real(8), save :: dx, d1, er0 = 1.0d-6
  integer, save :: i, dt = 1, nstep = 10, pstep=5
end module globals

subroutine eular_explicit(phi, n1, phi2, x)
  use globals
  implicit none
  !x, dt, d1, n1, nsteoの設定→この問題の場合は、横にしか点が動かない
  ! n1は分割数
  ! dxは分割数読み取った後のx1の差
  !dt=1, nstep=10
  real(8), intent(inout) :: phi(n1, 1), phi2(n1, 1), x(n1)
  integer, intent(in) :: n1
  integer istep
  character (len=3) :: fno
  character (len=40) :: fname
  call set_dbc(phi, n1, x)
  do istep = 0, nstep
    !計算ステップごとに出力ファイルを作成
    write(fno, fmt='(i3.3)')istep
    fname = 'output_6.21'//fno// '.d'
    !リスト6.7の計算方法
    do i = 2, n1 -1
      phi2(i, 1) = phi(i, 1) + d1 * (phi(i-1, 1) - 2.0d0 * phi(i, 1) + phi(i+1, 1))
    end do
    phi(:, 1) = phi2(:, 1)
    !各時間刻みごとに結果を出力する
    do i = 1, n1
      open(istep, file='output_6.21'//fno// '.d')
      write(istep , *) x(i), phi(i, 1)
    end do
    close(istep)
  end do
end subroutine eular_explicit


subroutine set_dbc(phi, n1, x)
  use globals
  implicit none
  real(8), intent(inout) :: phi(n1 ,1), x(n1)
  integer, intent(in) :: n1
  do i = 1, n1
    x(i) = dx * (i-1)
    write(*,*) x(i)
    !xが半分以下か以上かで初期条件が変わる
    if (x(i) <= 0.5d0) then
      phi(i, 1) = x(i)
    else if (x(i) > 0.5d0) then
      phi(i, 1) = 1 - x(i)
    end if
  end do
end subroutine set_dbc


program ensyu21
  use globals
  implicit none
  integer n1
  real(8), allocatable :: phi(:, :), phi2(:, :), x(:)
  write(*,*) ' input n1 '
  read(*,*) n1
  dx = 1.0d0 / (n1-1)
  d1 = 0.01d0 * dt / dx**2
  allocate(phi(n1, 1), phi2(n1, 1), x(n1))
  call eular_explicit(phi, n1, phi2, x)
end program ensyu21
