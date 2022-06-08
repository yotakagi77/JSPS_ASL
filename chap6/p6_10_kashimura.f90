module subprogs
    implicit none
  contains
  
  ! --- 最小二乗法によるn-1次への多項式回帰 ---
    subroutine least_squares(x,y,a,n)
    
      double precision,intent(in) :: x(:),y(:)
      double precision,allocatable,intent(out) :: a(:)
      double precision,allocatable :: b(:),c(:,:)
      integer :: n,m,i,j,k
      m=size(x,1) !データ数を調べる
      ! --- 次数n-1の設定 ---
      write(*,*) "n-1th order"
      write(*,'(a)',advance='no') ' input n = '
      read(*,*) n
      ! --- 最小二乗法の計算 ---
      allocate(a(n),b(n),c(n,n))
      b(:)=0
      c(:,:)=0
      do k=1,n
            do i=1,m
               b(k)=b(k)+y(i)*x(i)**(k-1)
            end do
         do j=1,n
            do i=1,m
               c(k,j)=c(k,j)+x(i)**(k+j-2)
            end do
         end do
      end do
      call gaussian_elimination_pv(c,a,b,n)
      b(:)=a(:)
      do k=1,n !次数が低い順で出力されるので、高次順に入れ替える
         a(n-k+1)=b(k)
      end do
      do i=1,n
         write(*,*)c(1:n,i)
      enddo

      write(*,*)b
    end subroutine
    
    subroutine gaussian_elimination_pv(a0,x,b,n)
      ! ---ガウスの消去法((部分pivot選択あり))---
      integer, intent(in) :: n                     ! 配列の寸法
      double precision, intent(in) :: a0(n,n),b(n) ! 形状明示配列
      double precision, intent(out) :: x(n)        ! 形状明示配列
      integer i,k, m
      double precision ar, am, t, a(n,n), w(n) ! a,wは作業用の自動割付け配列
      a(:,:) = a0(:,:)            ! 係数配列a0をaにコピー
      x(:)   = b(:)               ! 右辺ベクトルbをxにコピー
      !前進消去
      do k = 1, n
         ! --- 部分pivot選択
         m = k
         am = abs(a(k, k))
         do i = k + 1, n   ! a(i,k)の絶対値が最大となるm行を探す
            if (abs(a(i, k)) > am) then
               am = abs(a(i,k))
               m = i
            end if
         end do
         if (am == 0.0d0) stop 'A is singular'   ! Aが特異なら停止
         if (k /= m) then   ! k行とm行の入れ替え
            w(   k:n) = a(k, k:n)
            a(k, k:n) = a(m, k:n)
            a(m, k:n) = w(   k:n)
            t    = x(k)
            x(k) = x(m)
            x(m) = t
         end if
         ! --- 以下は通常のガガウスの消去法の演算
         if (a(k,k) == 0.0d0) stop ' pivot = 0' ! pivotが0なら停止する
         ar = 1.0d0 / a(k,k)            ! arは対角成分の逆数
         a(k,k)      = 1.0d0            ! 対角成分に1を設定
         a(k, k+1:n) = ar * a(k, k+1:n) ! k行目のk+1列からn列にarを乗ずる
         x(k)        = ar * x(k)        ! k行の右辺要素にもarを乗ずる
         do i = 1, n
            if (i > k) then   ! i行のk列からn列の要素とx(i)に対する演算
               a(i, k+1:n) = a(i, k+1:n) - a(i, k) * a(k, k+1:n)
               x(i)        = x(i)        - a(i, k) * x(k)
               a(i, k    ) = 0.0d0
            end if
         end do
      end do
      !後退代入
      do i = n, 1 ,-1
         x(i) = x(i) - dot_product(a(i,i+1:n), x(i+1:n))
      end do
    end subroutine gaussian_elimination_pv
    
  end module subprogs
  
  program main
  use subprogs
  
    implicit none
    double precision :: tmp1,tmp2,l,x1,y1
    double precision,allocatable :: x(:),y(:),a(:),x2(:)
    integer :: n,m,i,j
    
    open(10,file='output6_09_kashimura.d')
    open(20,file='p6_10_kashimura.txt')
    
    ! --- データ数を調べる ---
      m=0
      read(10,'()') !見出しの読み飛ばし
      do
         read(10,*,end=100) tmp1,tmp2 !読み込むデータに合わせて変更すること
         m=m+1
      end do
      100 continue
      rewind(10)  ! ファイルの最初に戻る
      read(10,'()') !見出しの読み飛ばし
      
    ! --- 点群の座標を配列に格納する ---
      allocate(x(m),y(m))
      do i=1,m
         read(10,*) j,x(i),y(i)
      end do
      write(*,'(a)',advance='no') ' x = '
      write(*,'(100e12.4)') x(:)
      write(*,'(a)',advance='no') ' y = '
      write(*,'(100e12.4)') y(:)
  
    ! --- 回帰多項式を計算 ---	
      call least_squares(x,y,a,n)
      write(*,'(a)',advance='no') '    coefficient a = '
      write(*,'(100e12.4)') a(:)
      
    ! --- 元の係数行列 ---
      write(*,'(a)',advance='no') 'raw coefficient a = '
      write(*,'(100e12.4)') 0.1d0,0.2d0,0.5d0,1.0d0
      
    ! --- 回帰多項式の概形を図示する ---
      allocate(x2(n))
      write(20,*) '#','         ','i','  ','x(i)','                      ','y(i)'
      x1=x(1)
      do j=1,n
           x2(j)=x1**(n-j)
      end do
      y1=dot_product(a,x2)
      write(20,*) i,x1,y1
      do i=1,99
         x1=x1+(x(m)-x(1))/99.d0
         do j=1,n
            x2(j)=x1**(n-j)
         end do
         y1=dot_product(a,x2)
         write(20,*) i,x1,y1
      end do
      
      
      
    close(10)
    close(20)
  end program main
  