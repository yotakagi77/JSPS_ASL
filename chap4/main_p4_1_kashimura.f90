program random_mat
  use interface_mod !インターフェイス・モジュールの使用宣言
  implicit none
  double precision,allocatable :: a(:,:)
  integer :: n
  write(*,'(a)',advance='no') ' input n : '
  read(*,*) n
  if(n<1 .or. n>100) stop 'n must be 0<n<100'
  allocate(a(n,n))
  call rmat(a,n) !乱数の設定
  call print_mat(a) !要素の値を出力(形状引継ぎ配列を利用)
end program random_mat