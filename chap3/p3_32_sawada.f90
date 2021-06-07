module subprogs
 implicit none
contains
 function triple(p,q,r) result(seki)
 real(8),intent(in)::p(:),q(:),r(:)
 real(8) seki,s(1:3)
 s(1)=q(2)*r(3)-r(2)*q(3)          !q‚Ær‚ÌŠOÏŒvZ
 s(2)=q(3)*r(1)-r(3)*q(1)
 s(3)=q(1)*r(2)-r(1)*q(2)
 seki=dot_product(p(:),s(:))       !p‚Æs‚Ì“àÏŒvZ
 end function triple
end module subprogs


program p3_32
 use subprogs
 implicit none
 real(8) a(1:3),b(1:3),c(1:3)
 write(*,*) 'Input a'
 read(*,*) a(1:3)
 write(*,*) 'Input b'
 read(*,*) b(1:3)
 write(*,*) 'Input c'
 read(*,*) c(1:3)
 write(*,*) 'ans=',triple(a,b,c)
end program p3_32
 