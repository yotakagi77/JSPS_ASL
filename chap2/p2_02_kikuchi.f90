program array02
    implicit none
    integer i 
    real(8) ia(1:4)
    !演算１（左端の要素に揃える）
    ia(1:4) = (/ 1, 2, 3, 4 /)
    do i = 2, 4
        ia(i) = ia(i-1)
    end do
    write(*,*) '演算１結果'
    write(*,*) 'ia(1:4) = ', ia(1:4)
    !演算２（要素を右に１つずらす）
    ia(1:4) = (/ 1, 2, 3, 4 /)
    ia(2:4) = ia(1:3)
    write(*,*) '演算２結果'
    write(*,*) 'ia(1:4) = ', ia(1:4)
end program array02