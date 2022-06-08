module subprogs
 implicit none
contains
 subroutine set_dbc(x, n1, n2, phi) !ディリクレ境界条件の設定
    integer   :: n1, n2
    real(8)   :: x(2,n1,n2), phi(n1,n2)
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

  subroutine chk_err(phi, c, d, n1, n2, er)
    integer i, j, n1, n2
    real(8) c, d, er, rhs
    real(8) phi(n1,n2)
    er = 0
    do j = 2, n2 - 1
     do i = 2, n1 - 1
      rhs = -c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-i) + phi(i, j+1))
      er = er + (rhs - phi(i,j)) ** 2
     enddo
    enddo
  end subroutine chk_err
end module subprogs

program main
 use subprogs
 implicit none
 integer i, j, itr, itrmax, n1, n2
 real(8), allocatable ::  x(:,:,:), phi(:,:)
 real(8)  omg, c, d, rhs, delta_1, delta_2, er, er0 
 itrmax = 10
 er0 = 1.0d-6
 write(*,*) 'input n1'
 read(*,*) n1
 write(*,*) 'input n2'
 read(*,*) n2
 !-------------各値の計算----------------------
 omg = 2 / (1.0d0 + sin(acos(-1.0d0) / (n1 - 1)))
 delta_1 = 1.0d0 / (n1 - 1.0d0) 
 delta_2 = 1.0d0 / (n2 - 1.0d0) 
 c = (-1.0d0) * (delta_2 ** 2) / (2 * (delta_1 ** 2 + delta_2 ** 2))
 d = c * (delta_1 / delta_2) ** 2
 write(*,*) omg, delta_1, delta_2, c, d
 !--------------------------------------------
 allocate (x(2,n1,n2), phi(n1,n2))
 call set_dbc(x, n1, n2, phi)  !ディリクレ境界条件の設定
 do itr = 1, itrmax
  do j = 2, n2 - 1
   do i = 2, n1 - 1
    rhs = -c * (phi(i-1, j) + phi(i+1, j)) - d * (phi(i, j-i) + phi(i, j+1))
    phi(i,j) = phi(i,j) + omg * (rhs - phi(i,j))
   enddo
  enddo
  call chk_err(phi, c, d, n1, n2, er)  !誤差をチェック
  write(*,*) 'itr, er =', itr, er  ! 途中経過の出力
  if(er < er0) exit !誤差がしきい値er0より小なら反復終了
 enddo
 do i = 1, n1
       write(*,*) (phi(i,j), j = 1, n2)
    enddo
end program main