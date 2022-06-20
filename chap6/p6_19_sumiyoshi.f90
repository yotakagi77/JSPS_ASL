module subprogs
 implicit none
contains
 subroutine set_dbc(x, n1, n2, phi) !ディリクレ境界条件の設定
    integer, intent(in)   :: n1, n2
    real(8), intent(out) :: phi(n1,n2), x(2,n1,n2)
    integer   :: i, j
    do i = 1, n1
     do j = 1, n2
      x(1,i,j) = 1.0d0 / dble(n1 - 1) * dble(i - 1)
      x(2,i,j) = 1.0d0 / dble(n2 - 1) * dble(j - 1)
     enddo
    enddo
    do i = 1, n1
     phi(i,1) = sin(acos(-1.0d0) * x(1,i,1))   !Φの設定
    enddo
    phi(2:n1-1, 2:n2-1) = 0.0d0
    do i = 1, n1
       write(*,*) (phi(i,j), j = 1, n2)
    enddo
  end subroutine set_dbc

  subroutine set_nbc(n1, n2, phi) !ノイマン境界条件の設定
    integer, intent(in) :: n1, n2
    real(8), intent(out) :: phi(n1,n2)
    integer :: i, j
    do i = 1, n1
     phi(i, n2) = phi(i, n2 - 1)
    enddo
  end subroutine set_nbc

  function chk_steady(phi, phi2, n1, n2) result(er)
    integer :: i, j, n1, n2
    real(8) :: phi(n1,n2), phi2(n1,n2), er  
    er = 0 
    do i = 1, n1
     do j = 1, n2
      er = er + (phi(i,j) - phi2(i,j)) ** 2
     enddo
    enddo
  end function chk_steady

  subroutine output(phi, n1, n2)
    integer :: i, j, n1, n2
    real(8) :: phi(n1,n2)
    do i = 1, n1
       write(*,*) (phi(i,j), j = 1, n2)
    enddo
  end subroutine output
end module subprogs

  program main
  use subprogs
  implicit none
  integer i, j, istep, pstep, n1, n2, nstep
  real(8), allocatable :: x(:,:,:), phi(:,:), phi2(:,:)
  real(8) alpha, dt, delta_1, delta_2, d1, d2, s, er, er0
  write(*,*) 'input n1, n2'
  read(*,*) n1, n2
  write(*,*) ' input predetermined time, pstep'
  read(*,*) pstep
  nstep = 10000
  er0 = 1.0d-6
  alpha = 0.5
  dt = 0.0001
  !-------------各値の計算----------------------
 delta_1 = 1.0d0 / (n1 - 1.0d0) 
 delta_2 = 1.0d0 / (n2 - 1.0d0) 
 d1 = alpha * dt / (delta_1 ** 2)
 d2 = alpha * dt / (delta_2 ** 2)
 s  = (delta_1 ** 2) * (delta_2 ** 2) / (2 * alpha * (delta_1 ** 2 + delta_2 ** 2))
 write(*,*)  delta_1, delta_2, d1, d2
 !--------------------------------------------
 allocate (x(2,n1,n2), phi(n1,n2), phi2(n1,n2))
 call set_dbc(x,n1, n2, phi)  !ディリクレ境界条件の設定
 call set_nbc(n1, n2, phi2)   !ノイマン境界条件の設定
 do istep = 1, nstep
  do j = 2, n2 - 1
   do i = 2, n1 - 1
    phi2(i,j) = phi(i,j) + d1 * (phi(i-1, j) - 2.0d0 * phi(i,j) + phi(i+1,j)) + d2 * (phi(i,j-1) - 2.0d0 * phi(i,j) + phi(i,j+1))
   enddo
  enddo
  call set_nbc(n1, n2, phi2)   !ノイマン境界条件の設定
  er = chk_steady(phi, phi2, n1, n2) !定常性のチェック
  phi(:,:) = phi2(:,:)  !結果の更新
  if(mod(istep, pstep) == 0) call output(phi, n1, n2) ! 途中結果出力
  if(er < er0) exit
 enddo
  open(1, file = 'output6_19.d')
  do j = 1, n2
   do i = 1, n1
       write(1,*) x(:,i,j), phi(i,j)
   enddo
   write(1,*) ''
  enddo
 end program main
