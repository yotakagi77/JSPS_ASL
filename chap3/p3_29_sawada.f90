module subprogs
 implicit none
contains
 function bai(m,i,j,c) result(p)  !i行i列をcとした行列を返す関数
 integer, intent(in)::m,i,j,c
 integer k,l
 real(8) p(m,m),imat(m,m)
  imat=0.0                       !単位行列をつくる
  do k=1,m
   imat(k,k)=1.0d0
  end do
  p(1:i-1,1:m)=imat(1:i-1,1:m)
  p(i,1:m)=c*imat(i,1:m)           !単位行列のi行をc倍
  p(i+1:m,1:m)=imat(i+1:m,1:m)
  write(*,*) 'p='
  do k=1,m
   write(*,*) (p(k,l),l=1,m)
  end do
 end function bai

 function kuwaeru(m,i,j,c) result(q)  !j行i列をcとした行列を返す関数
 integer, intent(in)::m,i,j,c
 integer k,l
 real(8) q(m,m),imat(m,m)
  imat=0.0
  do k=1,m
   imat(k,k)=1.0d0
  end do
  q(1:j-1,1:m)=imat(1:j-1,1:m)
  q(j,1:i-1)=imat(j,1:i-1)
  q(j,i)=c                            !j行i列の要素だけc
  q(j,i+1:m)=imat(j,i+1:m)
  q(j+1:m,1:m)=imat(j+1:m,1:m)
  write(*,*) 'q='
  do k=1,m
   write(*,*) (q(k,l),l=1,m)
  end do
 end function kuwaeru

 function irekae(m,i,j,c) result(r)    !i行とj行を入れ替えた行列を返す
 integer, intent(in)::m,i,j,c
 integer k,l
 real(8) r(m,m),imat(m,m),tmp1(1,m),tmp2(1,m)
  imat=0.0
  do k=1,m
   imat(k,k)=1.0d0
  end do
  r(1:m,1:m)=imat(1:m,1:m)
  tmp1(1,1:m)=r(i,1:m)                !i行を単位行列のj行にする
  r(i,1:m)=imat(j,1:m)
  imat(j,1:m)=tmp1(1,1:m)
  tmp2(1,1:m)=r(j,1:m)                !j行を単位行列のi行にする
  r(j,1:m)=imat(i,1:m)
  imat(i,1:m)=tmp2(1,1:m)
  write(*,*) 'r='
  do k=1,m
   write(*,*) (r(k,l),l=1,m)
  end do
 end function irekae
end module subprogs

!メイン
program p3_29
 use subprogs
 implicit none
 integer, parameter::m=3,n=3
 integer i,j,k,l,c
 real(8) a(1:m,1:n),pa(m,n),qa(m,n),ra(m,n) 
 write(*,*) 'Input A'
 read(*,*) a(1:m,1:n)
 do k=1,m
  write(*,*) (a(k,l),l=1,n)        !Aを表示
 end do
 write(*,*) 'Input i,j'
 read(*,*) i,j
 write(*,*) 'Input c'
 read(*,*) c
 pa(1:m,1:n)=matmul(bai(m,i,j,c),a)    !i行をc倍
 write(*,*) 'pa='
 do k=1,m
  write(*,*) (pa(k,l),l=1,n)
 end do
 qa(1:m,1:n)=matmul(kuwaeru(m,i,j,c),a)  !i行のc倍をj行に加える
 write(*,*) 'qa='
 do k=1,m
  write(*,*) (qa(k,l),l=1,n)
 end do
 ra(1:m,1:n)=matmul(irekae(m,i,j,c),a)   !i行とj行を入れ替える
 write(*,*) 'ra='
 do k=1,m
  write(*,*) (ra(k,l),l=1,n)
 end do
end program p3_29