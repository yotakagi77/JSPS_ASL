program ensyu6
implicit none
double precision :: alpha, beta, ganma, u1, u(3), v(3) = (/ 1.0, 1.0 ,1.0 /)

write(* , *) 'input u(1 : 3)'
read(* , *) u(1 : 3)
!ベクトルの大きさを求める
u1 = sqrt( u(1)**2 + u(2)**2 + u(3)**2 )
!答え
alpha = u(1) / u1
beta = u(2) / u1
ganma = u(3) / u1
write( * , *) alpha, beta, ganma

end program ensyu6