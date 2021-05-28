!inの値を変えたときはエラー出るけど、代入文を取ると0を返す
module subprog
 implicit none
contains
 subroutine swap(a, b, tmp)
  integer, intent(out) :: a, b, tmp
  tmp = a
  a = b
  b = tmp
  write(*,*) 'a, b, tmp = ', a, b, tmp
 end subroutine swap
end module subprog

program ensyu6
 use subprog
 implicit none
 integer :: x  = 77, y = 9095, tmp = 0
 write(*,*) 'x, y, tmp =', x, y, tmp
 call swap(x, y, tmp)
 write(*,*) 'x, y, tmp = ', x, y, tmp
end program ensyu6