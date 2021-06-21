module subprogs
 implicit none
contains
 subroutine gauss_seidel(a,b,x,n,itrmax,er0)
!a=係数行列,b=右辺ベクトル,itrmax=最大反復回数,er0=誤差のしきい値
  integer, intent(in)::n,itrmax
  real(8), intent(in)::a(n,n),b(n),er0
  real(8), intent(out)::x(n)
  real(8) s,er,rd(n),r(n)
  integer i,itr
  do i=1,n
   if (a(i,i) == 0.0d0) stop 'a(i,i) == 0.0d0'
   rd(i)=1.0d0/a(i,i) !対角要素が0でなければその逆数をrdとする
  end do
  x(1:n)=0.0d0  !初期解を0とする
  do itr=1,itrmax !反復計算のループ
   do i=1,n
    s=dot_product(a(i,1:i-1),x(1:i-1))
    s=s+dot_product(a(i,i+1:n),x(i+1:n))
    x(i)=rd(i)*(b(i)-s)
   end do
   r(1:n)=b(1:n)-matmul(a,x) !残差ベクトル
   er=dot_product(r,r)       !残差ベクトルの内積を誤差erとする
   write(*,*) 'itr=',itr,'err=',er
   if (er <= er0) then  !誤差erがしきい値er0以下なら反復計算終了
    write(*,*) '# converged #'
    exit !収束したら反復計算のループからぬける
   end if
  end do
 end subroutine gauss_seidel
 subroutine SOR(a,b,x,n,itrmax,er0)
!a=係数行列,b=右辺ベクトル,itrmax=最大反復回数,er0=誤差のしきい値
  integer, intent(in)::n,itrmax
  real(8), intent(in)::a(n,n),b(n),er0
  real(8), intent(out)::x(n)
  real(8) s,er,rd(n),r(n),omega
  integer i,itr
  write(*,'(a)',advance='no') ' omega = '
  read(*,*) omega
  do i=1,n
   if (a(i,i) == 0.0d0) stop 'a(i,i) == 0.0d0'
   rd(i)=1.0d0/a(i,i) !対角要素が0でなければその逆数をrdとする
  end do
  x(1:n)=0.0d0  !初期解を0とする
  do itr=1,itrmax !反復計算のループ
   do i=1,n
    s=dot_product(a(i,1:i-1),x(1:i-1))
    s=s+dot_product(a(i,i+1:n),x(i+1:n))
    x(i)=omega*(rd(i)*(b(i)-s)-x(i))+x(i)
   end do
   r(1:n)=b(1:n)-matmul(a,x) !残差ベクトル
   er=dot_product(r,r)       !残差ベクトルの内積を誤差erとする
   write(*,*) 'itr=',itr,'err=',er
   if (er <= er0) then  !誤差erがしきい値er0以下なら反復計算終了
    write(*,*) '# converged #'
    exit !収束したら反復計算のループからぬける
   end if
  end do
 end subroutine SOR

 subroutine set_dd_mat(a, n, b, x)
  real(8) a(n, n), s0, s, b(n), x(n)
  integer, intent(in) :: n
  integer i, j, k, seedsize
  integer, allocatable :: seed(:)
  call random_seed(size=seedsize) !初期値のサイズを取得
  allocate(seed(seedsize)) !配列の割り当て
  do k= 1, seedsize
   call system_clock(count=seed(k)) !時間を取得
  end do
  call random_seed(put=seed(:)) !初期値を与える
  call random_number(a(1:n, 1:n))
  deallocate(seed) 
!右辺のシグマの計算。1行ずつ優位かどうか判定する
  do i = 1, n
   s0 = 0.0d0
   s = 0.0d0
   do j = 1, n
    s0 = abs(a(i, j)) + s0
   end do 
!ここで1つの行の中の対角要素以外の和が完成する
   s = s0 -abs(a(i, i))
!もし優位じゃなかったらその行の要素を入れ替えてまた比較する無限ループ
   if ( abs(a(i, i)) <= s) then
    do
     call random_seed(size=seedsize) !初期値のサイズを取得
     allocate(seed(seedsize)) !配列の割り当て
     do k= 1, seedsize
      call system_clock(count=seed(k)) !時間を取得
     end do
     call random_seed(put=seed(:)) !初期値を与える
     call random_number(a(i, 1:n)) !該当の行に新しい乱数を入れる
     deallocate(seed)
     s0 = 0.0d0 !s,s0をリセットする
     s = 0.0d0
     do k = 1, n
      s0 = abs(a(i, k)) + s0
     end do 
     s = s0 -abs(a(i, i))
 !もし優位になったら無限ループから抜け出して、次の行を確認
     if ( abs(a(i, i)) > s) then
      write(*,*) i, ' abs(a(i, i)) > s '
      go to 1
     end if
    end do
!ループせずに次の行を確認
    else if ( abs(a(i, i)) > s) then
     write(*,*) i, ' abs(a(i, i)) > s '
    end if
   1 continue 
  end do
 write(*,*) 'a='
 do i = 1, n
  write(*,*) a(i, 1:n)
 end do
  call random_seed(size=seedsize) !初期値のサイズを取得
  allocate(seed(seedsize)) !配列の割り当て
  do k= 1, seedsize
   call system_clock(count=seed(k)) !時間を取得
  end do
  call random_seed(put=seed(:)) !初期値を与える
 call random_number(b(n))
 call random_number(x(n))
 write(*,*) 'b='
 do i = 1, n
  write(*,*) b(i)
 end do
 end subroutine set_dd_mat
end module subprogs

program p6_11
 use subprogs
 implicit none
 real(8), allocatable::a(:,:),b(:),x(:)
 integer::n,i,itrmax=100
 real(8)::er0=1.0d-6
 write(*,*) 'input n'
 read(*, *) n
 allocate (a(n,n),b(n),x(n))
 call set_dd_mat(a,n,b,x)
 call gauss_seidel(a,b,x,n,itrmax,er0)
 write(*,*) 'gauss_seidel method'
 write(*,'(a)',advance='no') ' x = '
 write(*,*) x(:)
 call SOR(a,b,x,n,itrmax,er0)
 write(*,*) 'SOR method'
 write(*,'(a)',advance='no') ' x = '
 write(*,*) x(:)
end program p6_11