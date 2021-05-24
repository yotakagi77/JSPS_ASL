program ensyu12_2
implicit none
real(8)  r, sn, tn, an
integer a, i, n
write(* , *) ' input n '
read(* , *) n
a = 16
r = 0.8
sn = a *(1-r**n) / (1-r)
tn = 0.0
 do i = 1, n
 tn = tn + a * r ** (i-1)
 end do
write(* ,*) sn, tn
end program ensyu12_2