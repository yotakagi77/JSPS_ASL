module exsubp_gname_interface
    interface ex_read_file_data

    subroutine read_file_idata(fno,title,ivar)
        character(*),intent(in):: title
        integer,intent(in)::fno
        integer,intent(out)::ivar(:,:)
    end subroutine read_file_idata

    subroutine read_file_idata_meiji(fno,title,ivar,im,jm)
        character(*),intent(in):: title
        integer,intent(in)::fno
        integer,intent(out)::ivar(im,jm)
        integer im,jm
    end subroutine read_file_idata_meiji

    subroutine read_file_i2data(fno,title,ivar1,ivar2)
        character(*),intent(in):: title
        integer,intent(in)::fno
        integer,intent(out)::ivar1,ivar2
    end subroutine read_file_i2data

    subroutine read_file_ddata(fno,title,dvar)
        character(*),intent(in):: title
        integer,intent(in)::fno
        real(kind(1d0)),intent(out)::dvar(:,:)
    end subroutine read_file_ddata

    subroutine read_file_ddata_meiji(fno,title,dvar,im,jm)
        character(*),intent(in):: title
        integer,intent(in)::fno
        real(kind(1d0)),intent(out)::dvar(im,jm)
        integer im,jm
    end subroutine read_file_ddata_meiji

    subroutine read_file_chardata(fno,title,charvar)
        character(*),intent(in):: title
        integer,intent(in)::fno
        character(*),intent(out)::charvar(:,:)
    end subroutine read_file_chardata

    subroutine read_file_chardata_meiji(fno,title,charvar,im,jm)
        character(*),intent(in):: title
        integer,intent(in)::fno
        character(*),intent(out)::charvar(im,jm)
        integer im,jm
    end subroutine read_file_chardata_meiji

    end interface
end module exsubp_gname_interface


!!!!!!!形状引継ぎ!!!!!!!
subroutine read_file_idata(fno,title,ivar)
    character(*),intent(in):: title
    integer,intent(in)::fno
    integer,intent(out)::ivar(:,:)
    integer im,jm,i
    im=size(ivar,1)
    jm=size(ivar,2)
    do i=1,im
        read(fno,*) ivar(i,:)
    end do
    write(*,*) title
    do i=1,im
    write(*,*) ivar(i,:)
    end do
end subroutine read_file_idata

subroutine read_file_idata_meiji(fno,title,ivar,im,jm)
    character(*),intent(in):: title
    integer,intent(in)::fno
    integer,intent(out)::ivar(im,jm)
    integer im,jm,i
    do i=1,im
        read(fno,*) ivar(i,:)
    end do
    write(*,*) title
    do i=1,im
    write(*,*) ivar(i,:)
    end do
end subroutine read_file_idata_meiji

subroutine read_file_i2data(fno,title,ivar1,ivar2)
    character(*),intent(in):: title
    integer,intent(in)::fno
    real(kind(1d0)),intent(out)::ivar1,ivar2
    read(fno,*) ivar1,ivar2
    write(*,*) title,ivar1,ivar2
end subroutine read_file_i2data

subroutine read_file_ddata(fno,title,dvar)
    character(*),intent(in):: title
    integer,intent(in)::fno
    real(kind(1d0)),intent(out)::dvar(:,:)
    integer im,jm,i
    im=size(dvar,1)
    jm=size(dvar,2)
    do i=1,im
        read(fno,*) dvar(i,:)
    end do
    write(*,*) title
    do i=1,im
    write(*,*) dvar(i,:)
    end do
end subroutine read_file_ddata

subroutine read_file_ddata_meiji(fno,title,dvar,im,jm)
    character(*),intent(in):: title
    integer,intent(in)::fno
    real(kind(1d0)),intent(out)::dvar(im,jm)
    integer im,jm
    do i=1,im
        read(fno,*) dvar(i,:)
    end do
    write(*,*) title
    do i=1,im
    write(*,*) dvar(i,:)
    end do
end subroutine read_file_ddata_meiji

subroutine read_file_chardata(fno,title,charvar)
    character(*),intent(in):: title
    integer,intent(in)::fno
    character,intent(out)::charvar(:,:)
    integer im,jm,i
    im=size(charvar,1)
    jm=size(charvar,2)
    do i=1,im
        read(fno,*) charvar(i,:)
    end do
    write(*,*) title
    do i=1,im
        write(*,*) charvar(i,:)
    end do
end subroutine read_file_chardata

subroutine read_file_chardata_meiji(fno,title,charvar,im,jm)
    character(*),intent(in):: title
    integer,intent(in)::fno
    character,intent(out)::charvar(im,jm)
    integer im,jm,i
    do i=1,im
        read(fno,*) charvar(i,:)
    end do
    write(*,*) title
    do i=1,im
        write(*,*) charvar(i,:)
    end do
end subroutine read_file_chardata_meiji

program read_data
    use exsubp_gname_interface
    implicit NONE
    integer :: fno=10
    integer,allocatable :: in(:,:)
    real(kind(1d0)),allocatable:: d(:,:)
    character,allocatable:: c(:,:)
    integer n
    open(fno,file="sample.d")
    read(fno,*) n
    allocate(d(n,n))
    allocate(c(n,n))
    allocate(in(n,n))

    open(fno,file="sample.d")
    call ex_read_file_data(fno,"int = ",in)
    call ex_read_file_data(fno,"double = ",d)
    call ex_read_file_data(fno,"character = ",c)
    close(fno)
    open(fno,file="sample.d")
    call ex_read_file_data(fno,"int(meiji) = ",in,n,n)
    call ex_read_file_data(fno,"double(meiji) = ",d,n,n)
    call ex_read_file_data(fno,"character(meiji) = ",c,n,n)

end program read_data


