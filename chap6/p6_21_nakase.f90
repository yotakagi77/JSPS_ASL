module globals
  real(8), save :: dx, d1, ramda, l = 1.0d0, alpha=0.01d0, dt = 1.0d0
  integer, save :: i, n, nstep = 10
  character (len=4) :: fno
  character (len=30) :: fname
end module globals

subroutine eular_explicit(phi, n1, phi2, x)
  use globals
  implicit none
  !x, dt, d1, n1, nsteoの設定→この問題の場合は、横にしか点が動かない
  ! n1は分割数
  ! dxは分割数読み取った後のx1の差
  !dt=1, nstep=10
  real(8), intent(inout) :: phi(n1), phi2(n1), x
  integer, intent(in) :: n1
  integer istep
  call set_dbc(phi, n1, x)
  do istep = 0, nstep
    !計算ステップが0のときは初期値をそのまま出力する
    if (istep == 0) then
      go to 1
    end if
    !リストと同様の計算方法
    do i = 2, n1-1
      phi2(i) = phi(i) + d1 * (phi(i-1) - 2.0d0 * phi(i) + phi(i+1))
    end do
    !x =0.5L のときの境界条件は1.24の求め方になる
    phi(:) = phi2(:)
    1 continue
    if (mod(istep, 1) == 0) call output(phi, x, n1, istep)
    !各時間刻みごとに結果を出力す
  end do
end subroutine eular_explicit

subroutine output(phi, x, n1, istep)
  use globals
  implicit none
  real(8), intent(in) :: phi(n1)
  real(8), intent(inout) :: x
  integer, intent(in) :: n1, istep
  !計算ステップごとに出力ファイルを作成
  write(fno, fmt='(i3.3)')istep
  fname = 'output_6.21'//fno// '.d'
  do i = 1, n1
    x = dx * (i-1)
    open(istep, file='output6.21_'//fno// '.d')
    write(istep , *) x, phi(i)
  end do
  close(istep)
end subroutine


subroutine set_dbc(phi, n1, x)
  use globals
  implicit none
  real(8), intent(inout) :: phi(n1), x
  integer, intent(in) :: n1
  do i = 1, n1
    x = dx * (i-1)
    !xが半分以下か以上かで初期条件が変わる
    if (i <= n1*0.5d0) then
      phi(i) = x
    else
      phi(i) = l - x
    end if
    write(*,*) x, phi(i)
  end do
end subroutine set_dbc


program ensyu21
  use globals
  implicit none
  integer n1
  real(8), allocatable :: phi(:), phi2(:)
  real(8) x
  write(*,*) ' input n1 '
  read(*,*) n1
  dx = 1.0d0 / (n1-1)
  d1 = (alpha * dt) / (dx*dx)
  write(*,*) d1
  write(*,*) ' '
  allocate(phi(n1), phi2(n1))
  call eular_explicit(phi, n1, phi2, x)
end program ensyu21
