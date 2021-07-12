!P179 演習6.22
module subprogs
  implicit none
contains

! ---------- 注意事項 ----------
!　以下の副プログラムは、
!　セットで用いられることが前提
!　使用の際は動作順に注意すること
! ----------------------------


  ! ----- 初期条件設定 ----- !

  subroutine set_lattice(dx1,dx2,n1,n2)  ! 格子の設定
    double precision,intent(out) :: dx1,dx2
    integer,intent(out) :: n1,n2
	! 分割数n1,n2の設定
	  write(*,'(a)',advance='no') ' input division number n1,n2 : '
      read(*,*) n1,n2
	  ! x1,x2平面の正方形領域(0<=x1,x2<=1)を想定し、dx1,dx2を設定
	  dx1=1.0d0/(dble(n1)-1.0d0)
	  dx2=1.0d0/(dble(n2)-1.0d0)
  end subroutine set_lattice

  subroutine set_dbc(phi,dx1,dx2,n1,n2)  ! ディリクレ境界条件の設定
    double precision,intent(in) :: dx1,dx2
    double precision,allocatable,intent(out) :: phi(:,:)
	integer,intent(out) :: n1,n2
	double precision :: pi
	integer :: i,j
	  pi=4.0d0*atan(1.0d0)
	  allocate(phi(n1,n2))  ! phiの割付け
	  phi(:,:)=0.0d0        ! SOR法で動作させるときの初期値設定
	  ! ディリクレ境界条件の設定
	  do i=1,n1
	     phi(i,1)  = sin(pi*dx1*(dble(i)-1.0d0))
	     phi(i,n2) = 0.0d0
	  end do
	  do j=1,n2
	     phi(1,j)  = 0.0d0
	     phi(n1,j)  = 0.0d0
	  end do
	  
  end subroutine set_dbc
  
  subroutine set_nbc(phi,n1,n2)  ! ノイマン境界条件の設定
	integer,intent(in) :: n1,n2
	double precision,intent(inout) :: phi(:,:)
	integer :: i,j
	  phi(:,n2-1) = phi(:,n2)  !x2=1の境界で勾配0
  end subroutine set_nbc
  
  ! ----- SOR法五点差分式 ----- !
  
  subroutine chk_err(phi,c,d,e,n1,n2,er)  ! 誤差を確認
    double precision,intent(in) :: phi(:,:),c,d,e
	integer,intent(in) :: n1,n2
	double precision,intent(out) :: er
    double precision :: rhs
	integer :: i,j
         er=0.0d0
		 do j=2,n2-1     ! 内部の格子点に対する
	        do i=2,n1-1  ! 演算を二重ループで行う
	  	       rhs = -c*(phi(i-1,j)+phi(i+1,j))&
		             -d*(phi(i,j-1)+phi(i,j+1))&
					 +e*phi(i,j)
		       er  = er+(rhs-phi(i,j))**2  ! 残差ベクトルの内積をerとするときはここで終了
		    end do
	     end do
		 er = sqrt(er)  ! 残差ベクトルの大きさを残差erとしている
  end subroutine chk_err
  
  subroutine set_omg(n1,n2,omg)  ! ラプラス方程式の差分法に対するomega0を設定(汎用)
    integer,intent(in) :: n1,n2
	double precision,intent(out) :: omg
	double precision :: pi
	  pi=4.0d0*atan(1.0d0)
	  if(n1/=n2 .or. n2<3) stop &
	     ' cannot set acceleration parameter omega0 for SOR method, n1/=n2 or n1,n2<3 '
		 ! この項を書き換えて手動設定などにするのも手
	  omg=2.0d0/(1.0d0+sin(pi/(dble(n1)-1.0d0)))
  end subroutine set_omg
  
  subroutine sor_5p(phi,phi2,c,d,e,er0,n1,n2)
    double precision,intent(inout) :: phi2(:,:)
	double precision,intent(in) :: er0,c,d,e,phi(:,:)
	integer,intent(in) :: n1,n2
	double precision :: omg,rhs,er
	integer :: i,j,itr,itrmax=100
	! phi:求める関数 dx1,dx2:各方向の刻み幅 n1,n2:各方向の分割数 itr:試行回数 itrmax:itrの上限値
	  call set_omg(n1,n2,omg)
      do itr=1,itrmax    ! SQR法の反復計算
         do j=2,n2-1     ! 内部の格子点に対する
	        do i=2,n1-1  ! 演算を二重ループで行う
	  	       rhs      = -c*(phi2(i-1,j)+phi2(i+1,j))&
		                  -d*(phi2(i,j-1)+phi2(i,j+1))&
						  +e*phi(i,j)
		       phi2(i,j) = phi2(i,j)+omg*(rhs-phi2(i,j))
		    end do
	     end do
   	     call chk_err(phi2,c,d,e,n1,n2,er)  ! 誤差を確認
!	     write(*,*) 'itr,er=',itr,er     ! 途中経過の出力
	     if(er<er0) exit  ! 誤差が閾値er0より小さければ反復終了
      end do
!	  call print_gnuplot(dx1,dx2,phi)
  end subroutine sor_5p
  
  ! ----- オイラー陰解法 -----
  
  subroutine chk_steady(phi,phi2,n1,n2,er)  ! 誤差を確認
    double precision,intent(in) :: phi(:,:),phi2(:,:)
	integer,intent(in) :: n1,n2
	double precision,intent(out) :: er
	integer :: i,j
      er=0.0d0
	  do j=1,n2       ! 内部の格子点に対する
	     do i=1,n1    ! 演算を二重ループで行う
		    er  = er+(phi2(i,j)-phi(i,j))**2  
		 end do
	  end do
      er = sqrt(er)
  end subroutine chk_steady
  
  subroutine euler_implicit
    double precision,allocatable :: phi(:,:),phi2(:,:)
	double precision :: a,c,dx1,dx2,d1,d2,e,f,dt,t,er,er0=1.0d-6
	integer :: i,j,istep,nstep=2000,pstep=1,n1,n2
	! phi:求める関数 dx1,dx2:各方向の刻み幅 n1,n2:各方向の分割数 istep:経過回数 nstep:istepの上限値
	  write(*,'(a)',advance='no') ' input diffusion number alpha : '
      read(*,*) a  ! 拡散係数alphaの設定
	  call set_lattice(dx1,dx2,n1,n2)  ! 格子の設定
      call set_dbc(phi,dx1,dx2,n1,n2)    ! ディリクレ境界条件の設定
	  call set_nbc(phi,n1,n2)  ! ノイマン境界条件の設定
	  allocate(phi2(n1,n2))
      phi2(:,:)=phi(:,:)  !phi2にも各条件を代入
	  !dt = dx1**2.0d0*dx2**2.0d0/(2.0d0*a*(dx1**2.0d0+dx2**2.0d0+1.0d-2))  !時間増分の設定
	  dt = 5.0d-4
	  d1 = a*dt/dx1**2.0d0
	  d2 = a*dt/dx2**2.0d0  
	  c  = (1.0d0+2.0d0*d1+2.0d0*d2)**(-1.0d0)
	  e  = -c*d1
	  f  = -c*d2
	  do istep=1,nstep
	     do j=2,n2-1     ! 内部の格子点に対する
	        do i=2,n1-1  ! 演算を二重ループで行う
			   call sor_5p(phi,phi2,e,f,c,er0,n1,n2)  !phi2をSOR法で解く
		    end do
	     end do
!		 if(mod(istep,pstep)==0) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==5) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==50) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==100) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==200) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==400) call output(phi,dx1,dx2,istep,n1,n2)
		 if(istep==2000) call output(phi,dx1,dx2,istep,n1,n2)
		 call chk_steady(phi,phi2,n1,n2,er)
		 phi(:,:)=phi2(:,:)
		 write(*,*) 'istep,er=',istep,er
!		 if(er<er0) exit
	  end do
	  call theory(dx1,dx2,n1,n2)
	  call print_gnuplot(dx1,dx2,phi)
  end subroutine euler_implicit
  
  ! ----- 理論解書き出し用 -----
  
  subroutine theory(dx1,dx2,n1,n2)
	double precision,intent(in) :: dx1,dx2
	integer,intent(in) :: n1,n2
	double precision :: f,x1,x2,pi,phi
	integer :: i,j
	open(13,file='theory.txt')
	pi = 4.0d0*atan(1.0d0)
	! x1=0.5d0のとき
	write(13,*) '#',' ','x1=0.5d0'
	write(13,*) '#',' ','x','                         ','phi(x1,x2)'
	do j=1,n2
	   x1 = 0.5d0
	   x2 = dx2*(dble(j)-1.0d0)
	   f  = sin(pi*x1)*sinh(pi*(1-x2))/sinh(pi)
	   write(13,*) x2,f
	end do
	write(13,'()')
	! x2=0.5d0のとき
	write(13,*) '#',' ','x2=0.5d0'
	write(13,*) '#',' ','x','                         ','phi(x1,x2)'
	do i=1,n1
	   x1 = dx1*(dble(i)-1.0d0)
	   x2 = 0.5d0
	   f  = sin(pi*x1)*sinh(pi*(1-x2))/sinh(pi)
	   write(13,*) x1,f
	end do
	close(13)
  end subroutine theory
  
  ! ----- グラフ描写用 ----- !
  
  subroutine print_gnuplot(dx1,dx2,phi)
    double precision,intent(in) :: phi(:,:),dx1,dx2
	double precision :: x1,x2
	integer :: i,j,n1,n2
	open(11,file='euler_implicit.txt')
	write(11,*) '#',' ','x1','                       ','x2','                         ','phi(x1,x2)'
	n1=size(phi,1)
	n2=size(phi,2)
	do i=1,n1
	   do j=1,n2
	      x1 = dx1*(i-1)
		  x2 = dx2*(j-1)
	      write(11,*) x1,x2,phi(i,j)
	   end do
	   write(11,'()')
	end do
	close(11)
  end subroutine print_gnuplot
  
  subroutine output(phi,dx1,dx2,istep,n1,n2)
    double precision,intent(in) :: phi(:,:),dx1,dx2
	double precision :: x1,x2
	integer :: i,j,n1,n2,istep
	character (len=4) :: time
  write (time,'(i4)') istep
	open(12,file='t='//time//'.txt')
	n1=size(phi,1)
	n2=size(phi,2)
	! x1=0.5d0のとき
	write(12,*) '#',' ','x1=0.5d0'
	write(12,*) '#',' ','x','                         ','phi(x1,x2)'
	do j=1,n2
	   i  = int(0.5d0/dx1+1.0d0)
	   x2 = dx2*(dble(j)-1.0d0)
	   write(12,*) x2,phi(i,j)
	end do
	write(12,'()')
	! x2=0.5d0のとき
	write(12,*) '#',' ','x2=0.5d0'
	write(12,*) '#',' ','x','                         ','phi(x1,x2)'
	do i=1,n1
	   j  = int(0.5d0/dx2+1.0d0)
	   x1 = dx1*(dble(i)-1.0d0)
	   write(12,*) x1,phi(i,j)
	end do
	close(12)
  end subroutine output

end module subprogs


program main
  use subprogs
  implicit none
    call euler_implicit
end program main

! ---------- memo ----------
! x1,x2平面の正方形領域(0<=x1,x2<=1)
! 与えられた境界条件の拡散方程式を解く
! オイラー陰解法