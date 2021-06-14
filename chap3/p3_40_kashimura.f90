!p119 演習3.40
module sample_mod
  implicit none
  private !モジュール内のすべての変数などは外部から参照できなくなる
  integer,save :: ia=1,ib=2,ic=3
  public ib,ic,sub !指定された変数などが外部から参照できるようになる
contains
  subroutine sub
    integer,save :: id=4
	write(*,*) id
  end subroutine sub
end module sample_mod

program chk_module
  use sample_mod,only : ib,sub !only句を付けると参照が制限される
  implicit none
  write(*,*) ib
  call sub
end program chk_module

!iaなどを出力させてコンパイルエラーも確認できる