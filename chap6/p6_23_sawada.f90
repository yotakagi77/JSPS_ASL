module subprogs
 implicit none
contains
!ディリクレ境界条件の設定
 subroutine set_dbc(phi,dx1,n1,n2)
  integer, intent(in) :: n1,n2
  real(8), intent(in) :: dx1
  real(8), intent(out) :: phi(n1,n2)
  integer i,j
  real(8) pi
  pi = 2.0d0 * acos(0.0d0)
  phi(:,:) = 0.0d0
  do j = 1,n2
   do i = 1,n1
    if (j == 1) then
     phi(i,j) = sin(pi * dx1 * dble(i-1))
    else if (j == n2) then
     phi(i,j) = 0.0d0
    else if (i == 1) then
     phi(i,j) = 0.0d0
    else if (i == n1) then
     phi(i,j) = 0.0d0
    end if
   end do
  end do
 end subroutine set_dbc

!ノイマン境界条件の設定
 subroutine set_nbc(phi,n1,n2)
  integer, intent(in) :: n1,n2
  real(8), intent(inout) :: phi(n1,n2)
  integer i
  do i = 1,n1
   phi(i,n2) = phi(i,n2-1)
  end do
 end subroutine set_nbc

!定常性のチェック
 function chk_steady(phi,phi2,n1,n2) result(er)
  integer, intent(in) :: n1,n2
  real(8), intent(in) :: phi(n1,n2),phi2(n1,n2)
  integer i,j
  real(8) er
  er = 0.0d0
  do j =1,n2
   do i =1,n1
    er = er + (phi(i,j)-phi2(i,j))**2
   end do
  end do
  er = sqrt(er)
 end function chk_steady

!途中結果の出力
 subroutine output(phi,dx1,dx2,istep,n1,n2)
  integer, intent(in) :: n1,n2,istep
  real(8), intent(in) :: phi(n1,n2),dx1,dx2
  real(8) x1,x2
  integer i,j
  character (len=4) :: time
  write(time,'(i4)') istep
  open(istep,file=time//'.txt')
  do j = 1,n2
   write(istep,*) ' '
   do i = 1,n1
    x1 = dx1 * (i-1)
    write(istep,*) x1,phi(i,j)
   end do
  end do
  close(istep)
 end subroutine output

!オイラー陰解法
 subroutine euler_implicit(phi,phi2,n1,n2,d1,d2,dx1,dx2,dt)
  integer, intent(in) :: n1,n2
  real(8), intent(in) :: d1,d2,dt
  real(8), intent(inout) :: phi(n1,n2),phi2(n1,n2),dx1,dx2
  integer istep,i,j
  integer, parameter :: nstep = 1000, pstep = 100
  real(8) er,er0,gamma,e,f,t1,t2
  call cpu_time(t1)
  er0 = 1.0d-6
  phi(2:n1-1,2:n2-1) = 0.0d0
  call set_dbc(phi,dx1,n1,n2)
  call set_nbc(phi,n1,n2)
  gamma = (1.0d0 + 2.0d0 * d1 + 2.0d0 * d2)**(-1.0d0)
  e = - gamma * d1
  f = - gamma * d2
  phi2(:,:) = phi(:,:)
  do istep = 1,nstep
   do j = 2,n2-1
    do i = 2,n1-1
     phi2(i,j) = - e * (phi2(i-1,j) + phi2(i+1,j)) &
                 - f * (phi2(i,j-1) + phi2(i,j+1)) &
                 + gamma * phi(i,j)
    end do
   end do
   call set_nbc(phi2,n1,n2)
   er = chk_steady(phi,phi2,n1,n2)
   phi(:,:) = phi2(:,:)
   if (mod(istep,pstep) == 0) call output(phi,dx1,dx2,istep,n1,n2)
   write(*,*) 'istep,er=',istep,er
   if (er/dt < er0) exit
  end do
 call cpu_time(t2)
 write(*,*) 'cpu_time = ',t2-t1
 end subroutine euler_implicit
end module subprogs

program p6_20_2
 use subprogs
 implicit none
 integer, parameter :: n1 = 31, n2 = 31
 real(8), parameter :: dt = 0.0005d0, alpha = 0.5d0
 real(8) phi(1:n1,1:n2),phi2(1:n1,1:n2),x,d1,d2,dx1,dx2
 dx1 = 1.0d0 / (n1-1)
 dx2 = 1.0d0 / (n2-1)
 d1 = (alpha * dt) / dx1**2
 d2 = (alpha * dt) / dx2**2
 call euler_implicit(phi,phi2,n1,n2,d1,d2,dx1,dx2,dt)
end program p6_20_2