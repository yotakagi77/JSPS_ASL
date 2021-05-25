program ensyu2
implicit none
integer n, a(10), b(10), c(10)
write(* , *) 'input n'
read(* , *) n
write(* , *) 'input a(n)'
read(* , *) a(1 : n)
write(* , *) 'input b(n)'
read(* , *) b(1 : n)
c(1 : n) = ( a(1 : n ) - b(1 : n) )**2
write(* , '(5i6)' ) c(1 : n)
end program ensyu2