module subprogs
 implicit none
contains
 subroutine bicgstab1d(a,b,x,n,itrmax,er0)
!nはxの要素数,itrmaxは最大反復回数,er0は収束判定のしきい値
  integer, intent(in)::n,itrmax
  real(8), intent(in)::a(n,n),b(n),er0
  real(8), intent(out)::x(n)
  integer itr
  real(8) alp,bet,c1,c2,c3,ev,vv,rr
  real(8) r(n),r0(n),p(n),y(n),e(n),v(n)
!初期値の設定
  x(:)=0.0d0           !初期の解ベクトルをゼロベクトルとする
  r(:)=b-matmul(a,x)   !初期残差ベクトル
  c1=dot_product(r,r)  !初期残差ベクトルの内積
  if (c1 < er0) return
  p(:)=r(:)            !初期修正方向ベクトル
  r0(:)=r(:)           !初期残差ベクトルをr0として保存
  do itr=1,itrmax      !最大itrmaxまで反復計算
    y(:)=matmul(a,p)             !行列ベクトル積
    c2=dot_product(r0,y)         !ベクトルの内積
    alp=c1/c2
    e(:)=r(:)-alp*y(:)
    v(:)=matmul(a,e)             !行列ベクトル積
    ev=dot_product(e,v)          !ベクトルの内積
    vv=dot_product(v,v)          !ベクトルの内積
    c3=ev/vv
    x(:)=x(:)+alp*p(:)+c3*e(:)   !解ベクトルの更新
    r(:)=e(:)-c3*v(:)            !残差ベクトルの更新
    rr=dot_product(r,r)          !残差ベクトルの内積
    write(*,*) 'itr,er=',itr,rr
    if (rr < er0) exit           !誤差が小さければ終了     
    c1=dot_product(r0,r)         !ベクトルの内積
    bet=c1/(c2*c3)
    p(:)=r(:)+bet*(p(:)-c3*y(:)) !修正方向ベクトルの更新
  end do
 end subroutine bicgstab1d

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

program p6_14
 use subprogs
 implicit none
 real(8), allocatable::a(:,:),b(:),x(:)
 integer::n,itrmax=100
 real(8)::er0=1.0d-6
 write(*,*) 'Input n'
 read(*,*) n
 allocate (a(n,n),b(n),x(n))
 call set_dd_mat(a,n,b,x)
 call bicgstab1d(a,b,x,n,itrmax,er0)
 write(*,*) 'x=',x(:)
 deallocate(a,b,x)
end program p6_14