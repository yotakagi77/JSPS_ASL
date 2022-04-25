program loop_p_c
  implicit none
  integer n, j, i, k, p, p1, p2, p3, c, r
  
    write(*,*) ' input n (input 0 to stop) : '
    read(*,*) n

    write(*,*) ' input r (input 0<r<n) : '
    read(*,*) r

    if(n==0)then
      write(*,*) ' sorry, input positive n ...'
    else if(n<0) then
      write(*,*) ' sorry, input positive n ...'
    else if(n>10) then
      write(*,*) ' sorry, input n under 10 ...'
    else if(r>n) then
      write(*,*) ' sorry, imput r again ...' 
    else
      p1=1
      p2=1
      p3=1
      do i = 2, n
         p1 = p1 * i
      enddo

      do j = 1, n-r
         p2 = p2 * (n-r-j+1)
      enddo
 
      do k = 1, r
         p3 = p3 * k
      enddo
      
      p = p1 / p2
      c = p / p3
      

     endif
     write(*,*) 'p = ', p
     write(*,*) 'c = ', c

end program loop_p_c






      
        