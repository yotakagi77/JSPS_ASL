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

  subroutine chk_err(phi,c,d,e,n1,n2,er)  ! 誤差を確認
  integer,intent(in) :: n1,n2
  real(8),intent(in) :: phi(:,:),c,d,e
  real(8),intent(out) :: er
  real(8) :: rhs
  integer :: i,j
  er=0.0d0
  do j=2,n2-1     ! 内部の格子点に対する
   do i=2,n1-1  ! 演算を二重ループで行う
    rhs = -c*(phi(i-1,j)+phi(i+1,j))&
       -d*(phi(i,j-1)+phi(i,j+1)) +e*phi(i,j)
     er  = er+(rhs-phi(i,j))**2  ! 残差ベクトルの内積をerとするときはここで終了
   enddo
  enddo
  er = sqrt(er)  ! 残差ベクトルの大きさを残差erとしている

  end subroutine chk_err


  subroutine set_omg(n1,n2,omg)  ! ラプラス方程式の差分法に対するomega0を設定(汎用)
   integer,intent(in) :: n1,n2
   real(8),intent(out) :: omg
   real(8) :: pi
   pi=4.0d0*atan(1.0d0)
   if(n1/=n2 .or. n2<3) stop &
  ' cannot set acceleration parameter omega0 for SOR method, n1/=n2 or n1,n2<3 '
   omg=2.0d0/(1.0d0+sin(pi/(dble(n1)-1.0d0)))

  end subroutine set_omg



  subroutine sor_5p(phi,phi2,c,d,e,er0,n1,n2)
   integer,intent(in) :: n1,n2
   real(8), intent(out) :: phi2(n1,n2)
   real(8),intent(in) :: er0,c,d,e,phi(n1,n2) 
   real(8) :: omg,rhs,er,pi
   integer :: i,j,itr,itrmax=100
   ! phi:求める関数 dx1,dx2:各方向の刻み幅 n1,n2:各方向の分割数 itr:試行回数 itrmax:itrの上限値
   call set_omg(n1,n2,omg)
    do itr=1,itrmax    ! SQR法の反復計算
     do j=2,n2-1     ! 内部の格子点に対する
      do i=2,n1-1  ! 演算を二重ループで行う
       rhs  = -c*(phi2(i-1,j)+phi2(i+1,j))&
        -d*(phi2(i,j-1)+phi2(i,j+1)) +e*phi(i,j)
       phi2(i,j) = phi2(i,j)+omg*(rhs-phi2(i,j))
      enddo
     enddo
     call chk_err(phi2,c,d,e,n1,n2,er)  ! 誤差を確認

!            write(*,*) 'itr,er=',itr,er     ! 途中経過の出力

     if(er<er0) exit  ! 誤差が閾値er0より小さければ反復終了
    enddo
  end subroutine sor_5p

  subroutine print_gnuplot(x,phi,n1,n2)
   integer, intent(in) :: n1,n2
   real(8),intent(in) :: phi(n1,n2),x(2,n1,n2)
   integer :: i,j
    open(1,file='euler_implicit.txt')
     write(11,*) '#',' ','x1','                       ','x2','                         ','phi(x1,x2)'
      do j = 1, n2
       do i = 1, n1
        write(1,*) x(:,i,j), phi(i,j)
       enddo
       write(1,*) ''
      enddo
    close(11)

  end subroutine print_gnuplot
   

end module subprogs

  program main
  use subprogs
  implicit none
  integer i, j, istep, pstep, n1, n2, nstep
  real(8), allocatable :: x(:,:,:), phi(:,:), phi2(:,:), phi3(:,:), phi4(:,:)
  real(8) alpha, dt, delta_1, delta_2, d1, d2, s, er, er0, gamma, f, e, t1,t2,t3,t4
  write(*,*) 'input n1, n2'
  read(*,*) n1, n2
  nstep = 2000
  pstep = 500
  er0 = 1.0d-6
  alpha = 0.5
  dt = 0.5
  !-------------各値の計算----------------------
 delta_1 = 1.0d0 / (n1 - 1.0d0) 
 delta_2 = 1.0d0 / (n2 - 1.0d0) 
 d1 = alpha * dt / (delta_1 ** 2)
 d2 = alpha * dt / (delta_2 ** 2)
 s  = (delta_1 ** 2) * (delta_2 ** 2) / (2 * alpha * (delta_1 ** 2 + delta_2 ** 2))
 gamma = (1 + 2 * d1 + 2 * d2) ** (-1)
 e = (-1) * gamma * d1
 f = (-1) * gamma * d2
 allocate (x(2,n1,n2), phi(n1,n2), phi2(n1,n2), phi3(n1,n2),phi4(n1,n2))
 !--------陰解法------------------------------
 call cpu_time(t1)
 call set_dbc(x,n1, n2, phi)  !ディリクレ境界条件の設定
 call set_nbc(n1, n2, phi)   !ノイマン境界条件の設定
 phi2(:,:) = phi(:,:)
 do istep = 1, nstep
  call sor_5p(phi,phi2,e,f,gamma,er0,n1,n2)  !phi2をSOR法で解く
   do j=2,n2-1     ! 内部の格子点に対する
    do i=2,n1-1  ! 演算を二重ループで行う

    end do
   end do
  er = chk_steady(phi, phi2, n1, n2) !定常性のチェック
  phi(:,:) = phi2(:,:)  !結果の更新
  if(mod(istep, pstep) == 0) call output(phi, n1, n2) ! 途中結果出力
  if(er < er0) exit !誤差がしきい値er0より小なら反復終了
 enddo
 call cpu_time(t2)
 open(1, file = 'output6_22.d')
  do j = 1, n2
   do i = 1, n1
       write(1,*) x(:,i,j), phi(i,j)
   enddo
   write(1,*) ''
  enddo
!-----------陽解法----------------------------------
 call cpu_time(t3)
 call set_dbc(x,n1, n2, phi3)  !ディリクレ境界条件の設定
 call set_nbc(n1, n2, phi3)   !ノイマン境界条件の設定
 phi4(:,:) = phi3(:,:)
 do istep = 1, nstep
  do j = 2, n2 - 1
   do i = 2, n1 - 1
    phi4(i,j) = phi3(i,j) + d1 * (phi3(i-1, j) - 2.0d0 * phi3(i,j) &
    + phi3(i+1,j)) + d2 * (phi3(i,j-1) - 2.0d0 * phi3(i,j) + phi3(i,j+1))
   enddo
  enddo
  call set_nbc(n1, n2, phi2)   !ノイマン境界条件の設定
  er = chk_steady(phi, phi2, n1, n2) !定常性のチェック
  phi3(:,:) = phi4(:,:)  !結果の更新
  if(mod(istep, pstep) == 0) call output(phi, n1, n2) ! 途中結果出力
  if(er < er0) exit !誤差がしきい値er0より小なら反復終了
 enddo
 call cpu_time(t4)
 write(*,*) 'yang', t2-t1
 write(*,*) 'yin', t4-t3

 end program main


 