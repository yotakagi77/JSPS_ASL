!p82 演習3.4
module subprog
  implicit none
contains
  subroutine count_(ic)
    integer :: ic
    integer,save :: ic_1=0
	!宣言のときに初期値を設定すると、プログラム開始時しか初期化されない
	!saveを宣言しない場合でも同様に動作するが、明記した方が親切
	!ループする際にも毎回初期化するときは、宣言のときとは別で記述する
	ic_1=ic_1+1
	ic=ic_1
  end subroutine count_
end module subprog



program main
  use subprog
  implicit none
  
  integer :: i
  integer,save :: ic=0
  open(1,file='output.txt')

  do i=1,10
     call count_(ic)
	 write(1,*) ic
  end do
  
  close(1)
  
stop
end program main