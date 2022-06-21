module subprogs
    implicit none
    contains

    subroutine set_init(n1,n2,dt,xl,dx,gr,itrmax,h0,dh,&
        x,eta,c)
        real(8) dt,xl,gr,h0,dh
        integer n1,n2,itrmax,i, j
        real(8),allocatable :: x(:,:,:),eta(:,:,:),dx(:),c(:)
        open(10,file="data6_25.txt")
        read(10,*)n1,n2,itrmax
        read(10,*)dt,xl,gr,h0
        close(10)
        allocate(dx(2),x(2,n1,n2),eta(n1,n2,3),c(2))
        dx(1)=xl/dble(n1-1)
        dx(2)=xl/dble(n2-1)
        dh=0.1d0*h0
        do i = 1, n1
         do j = 1, n2
          x(1,i,j) = 1.0d0 / dble(n1 - 1) * dble(i - 1)
          x(2,i,j) = 1.0d0 / dble(n2 - 1) * dble(j - 1)
         enddo
        enddo
        do i = 1,n1
         do j = 1, n2
          eta(i,j,1)=h0+dh*exp(-(x(1,i,j)-0.5d0*xl)**2/1.0d2)
          eta(i,j,2)=h0+dh*exp(-(x(1,i,j)-0.5d0*xl)**2/1.0d2)
         enddo
        enddo

    end subroutine set_init

end module subprogs

program main
 use subprogs
 implicit none
 real(8),allocatable :: x(:,:,:), eta(:,:,:),dx(:),c(:)
 real(8) dt,xl,gr,h0,dh,c0
 integer n1,n2,itr,itrmax,i,j
 call set_init(n1,n2,dt,xl,dx,gr,itrmax,h0,dh,&
    x,eta,c)
 c(1:2) = gr * h0 * dt ** 2 / dx(1:2) ** 2
 c0 = 2.0d0 * (1.0d0 - c(1) - c(2))
 do itr = 1, itrmax
  do j = 2, n2-1
   do i = 2, n1-1
    eta(i,j,3) = c0 * eta(i,j,2) - eta(i,j,1) &
    + c(1) * (eta(i-1,j,2) + eta(i+1,j,2)) + c(2) * (eta(i,j-1,2) + eta(i,j+1,2))
   enddo
  enddo
  eta(2:n1-1,2:n2-1,1) = eta(2:n1-1,2:n2-1,2) !内部領域の結果の更新
  eta(2:n1-1,2:n2-1,2) = eta(2:n1-1,2:n2-1,3) !同上
  eta(1,:,:) = eta(2,:,:) !境界条件の設定
  eta(n1,:,:) = eta(n1-1,:,:) !同上
  eta(:,1,:) = eta(:,2,:) !同上
  eta(:,n2,:) = eta(:,n2-1,:) !同上
 enddo
 open(20, file = 'output6_25.d')
  do j = 1, n2
   do i = 1, n1
       write(20,*) x(:,i,j), eta(i,j,3)
   enddo
   write(20,*) ''
  enddo
 end program main