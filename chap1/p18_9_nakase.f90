program ensyu9
 implicit none
  integer  a, b
  integer  m, n, k
 do
  write(*,*) 'Input two positive integer (m , n):'
  read(*,*) a, b
  if(a > 10000 .or. b > 10000) then 
   write(* , *) ' put under 10000'
   cycle
  else 
   exit
  end if
  end do

  m = a
  n = b
  do
     k = mod(m, n)
     if (k == 0) then
        write(*,*) 'kouyakusuu ha ', n
        exit
     end if
     m = n
     n = k
  end do

  stop
end