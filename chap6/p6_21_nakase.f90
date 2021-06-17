module globals
  real(8), save :: dx, d1, ramda, sum, sumi, pi, k, ex, s, er0 = 1.0d-6, l = 1.0d0
  integer, save :: i, n, dt = 1, nstep = 10, pstep=5
end module globals

subroutine eular_explicit(phi, n1, phi2, x)
  use globals
  implicit none
  !x, dt, d1, n1, nsteoの設定→この問題の場合は、横にしか点が動かない
  ! n1は分割数
  ! dxは分割数読み取った後のx1の差
  !dt=1, nstep=10
  real(8), intent(inout) :: phi(n1, 1), phi2(n1, 1), x
  integer, intent(in) :: n1
  integer istep
  character (len=3) :: fno
  character (len=40) :: fname
  call set_dbc(phi, n1, x)
  do istep = 0, nstep
    call set_nbc(phi, x, n1)
    !計算ステップごとに出力ファイルを作成
    write(fno, fmt='(i3.3)')istep
    fname = 'output_6.21'//fno// '.d'
    !計算ステップが0のときは初期値をそのまま出力する
    if (istep == 0) then
      go to 1
    end if
    !リストと同様の計算方法
    do i = 2, n1 -1
      phi2(i, 1) = phi(i, 1) + d1 * (phi(i-1, 1) - 2.0d0 * phi(i, 1) + phi(i+1, 1))
    end do
    !x =0.5L のときの境界条件は1.24の求め方になる
    call set_nbc(phi2, x, n1)
    phi(:, 1) = phi2(:, 1)
    1 continue
    !各時間刻みごとに結果を出力する
    do i = 1, n1
      x = dx * (i-1)
      open(istep, file='output_6.21_2'//fno// '.d')
      write(istep , *) x, phi(i, 1)
    end do
    close(istep)
  end do
end subroutine eular_explicit

subroutine set_dbc(phi, n1, x)
  use globals
  implicit none
  real(8), intent(inout) :: phi(n1 ,1), x
  integer, intent(in) :: n1
  do i = 1, n1
    x = dx * (i-1)
    write(*,*) x
    !xが半分以下か以上かで初期条件が変わる
    if (x <= 0.5d0) then
      phi(i, 1) = x
    else if (x > 0.5d0) then
      phi(i, 1) = 1 - x
    end if
  end do
end subroutine set_dbc

subroutine set_nbc(phi, x, n1)
  use globals
  implicit none
  real(8), intent(inout) :: phi(n1 ,1), x
  integer, intent(in) :: n1
  integer istep
  do i = 1, n1
    x = dx * (i-1)
    if (x == 0.5d0 * 1.0d0) then
      k = 0.01d0
      pi = 2.0d0 * acos(0.0d0)
      sum = 0.0d0
      do n = 1, 50
        ramda = k * (n * pi / l)
        !expの部分
        ex = (exp(-ramda**2 * istep) * (-1) ** (n-1)) / (2 * n - 1)** 2
         !sin の部分
         s = sin(pi * x / l)
         !sin と expをかける
         sumi = ex * s
         !ここで、1個前までの項の級数が残る
         sum = sum + sumi
      end do
      phi(i, 1) = (4.0d0 / pi ** 2) * sum
    end if
  end do
end subroutine set_nbc

program ensyu21
  use globals
  implicit none
  integer n1
  real(8), allocatable :: phi(:, :), phi2(:, :)
  real(8) x
  write(*,*) ' input n1 '
  read(*,*) n1
  dx = 1.0d0 / (n1-1)
  d1 = 0.01d0 * dt / dx**2
  allocate(phi(n1, 1), phi2(n1, 1))
  call eular_explicit(phi, n1, phi2, x)
end program ensyu21
