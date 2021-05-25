program ensyu3_2
implicit none
integer n, a(10), b(10), c(10), i
write(* , *) 'input n'
read(* , *) n
write(* , *) 'input a(n)'
read(* , *) a(1 : n)
write(* , *) 'input b(n)'
read(* , *) b(1 : n)
do i = 1, n
 c(i) = a(i) * a(i)  - 2 * a(i) * b(i) + b(i) * b(i)
end do
write(* , * ) (c(i), i = 1, n)
end program ensyu3_2