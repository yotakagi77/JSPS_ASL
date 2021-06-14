module subprogs
 implicit none
contains

 function normal_vec2(v,n) result(nv)   !正規化ベクトルを返す関数
  integer, intent(in)::n
  real(8), intent(in)::v(n)
  real(8) nv(n),vl
  vl=sqrt(dot_product(v,v))
  if (vl == 0.0d0) then
   nv(:)=0.0d0
  else
   nv(:)=v(:)/vl
  end if
 end function normal_vec2


 function gs(a,n) result(e)           !直交行列を返す関数
  integer, intent(in)::n
  real(8), intent(in)::a(1:n,1:n)
  real(8) e(1:n,1:n),dotp
  integer k,j
  e(1:n,1)=normal_vec2(a(1:n,1:1),n)
  do k=2,n
   e(1:n,k)=a(1:n,k)
   do j=1,k-1
    dotp=dot_product(a(1:n,k),e(1:n,j))
    e(1:n,k)=e(1:n,k)-dotp*e(1:n,j)
   end do
   e(1:n,k)=normal_vec2(e(1:n,k:k),n)
  end do
 end function gs


 function func_det(a,i) result(det)     !行列式を返す関数
  integer, intent(in)::i
  integer j
  real(8), intent(in)::a(1:3,1:3)
  real(8) d,e,b(1:2,1:3),c(1:2,1:2),det 
  b(1:i-1,1:3)=a(1:i-1,1:3)     !i行目を抜く
  b(i:2,1:3)=a(i+1:3,1:3)
  det=0.0d0
  do j=1,3                      !j列目を抜いて行列式出すまでをループ
   c(1:2,1:j-1)=b(1:2,1:j-1)
   c(1:2,j:2)=b(1:2,j+1:3)
   d=c(1,1)*c(2,2)-c(1,2)*c(2,1) !小行列の行列式
   e=(-1)**(i+j)*d               !余因子
   det=det+a(i,j)*e              !行列式の計算
  end do
 end function func_det
end module subprogs


program p3_38
 use subprogs
 implicit none
 integer, parameter::n=3,i=1
 integer j,k
 real(8) a(1:n,1:n),e(1:n,1:n)
 call random_seed
 call random_number(a(1:n,1:n))
  write(*,*) 'A='
  do j=1,n
   write(*,*) (a(j,k),k=1,n)
  end do
   e(:,:)=gs(a,n)
   write(*,*) 'T='
   do j=1,n
    write(*,*) (e(j,k),k=1,n)
   end do
   write(*,*)'|T|=',func_det(e,i)
end program p3_38