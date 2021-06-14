module subprog
 implicit none
contains
 subroutine set_dd_mat(a, n)
  real(8) a(n, n), s0, s
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
  do i = 1,n
   write(*,*) a(i, 1:n)
  end do
  do i = 1, n
   a(i, i) = 1.0d0
  end do
  do i = 1,n
   write(*,*) a(i, 1:n)
  end do
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
     a(i, i) =1.0d0 !対角要素は1にする
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
 do i = 1, n
  write(*,*) a(i, 1:n)
 end do
 end subroutine set_dd_mat
end module

program ensyu4
 use subprog
 implicit none
 real(8), allocatable :: a(:, :)
 integer n
 write(*,*) ' input n '
 read(*,*) n
 allocate(a(n, n))
 call set_dd_mat(a, n)
end program ensyu4