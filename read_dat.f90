program main
  implicit none
  call example1()

contains

  subroutine read_3d(no,u,nx,ny,nz)
    integer :: no, nx, ny, nz, k
    real(kind=8), dimension(nx, ny, nz) :: u
    real(kind=8), dimension(nx,ny) :: u2d
    do k=1,nz
      read(no) u2d
      u(:,:,k)=u2d
    enddo
  end

  subroutine example1()
    ! use tecplot module
    use tecplot

    ! define a tecplot object
    type(tecplot_time_file) :: plt_file
    integer,allocatable :: locations(:)
    integer,allocatable :: type_list(:)
    integer,allocatable :: shared_list(:)
    integer :: istep
    real(kind=8) :: tt
    !===================== USER DEFINED ======================================
    integer,parameter :: num_of_variables = 5
    !===================== USER DEFINED ======================================

    integer :: nx,ny,nz
    character(len=50) :: filename='test.plt'
    real(kind=4),allocatable :: your_datas(:,:,:,:)
    !===================== USER DEFINED ======================================
    real(kind=8),allocatable, dimension(:,:,:) :: xx3d, yy3d, zz3d, Q, d, u,v,w,T
    !===================== USER DEFINED ======================================

    real(kind=4) :: physics_time = 1.0

    ! set dimensions
    !===================== USER DEFINED ======================================
    nx = 1600
    ny = 1600
    nz = 120
    !===================== USER DEFINED ======================================

    allocate(your_datas(nx,ny,nz,num_of_variables))
    allocate(locations(num_of_variables))
    allocate(type_list(num_of_variables))
    allocate(shared_list(num_of_variables))
    allocate(xx3d(nx,ny,nz), yy3d(nx,ny,nz), zz3d(nx,ny,nz),Q(nx,ny,nz), U(nx,ny,nz), d(nx,ny,nz))
    allocate(v(nx,ny,nz), w(nx,ny,nz), T(nx,ny,nz))
    ! read data file
    print*, "Read grid file ......"
    open(56,file='OCFD3d-Mesh.dat',form='unformatted')
    call read_3d(56,xx3d,nx,ny,nz)
    call read_3d(56,yy3d,nx,ny,nz)
    call read_3d(56,zz3d,nx,ny,nz)
    close(56)

    !===================== USER DEFINED ======================================
    print*, 'read data file ...'
    open(70,file='Q-0360000.dat',form='unformatted')
    print*, 'read Q ...'
    call read_3d(70,Q,nx,ny,nz)
    close(70)
    print*, 'read data ok'

    open(70,file='OCFD00360000.dat.average',form='unformatted')
    read(70) Istep, tt
    print*, 'read primitives ...'
    call read_3d(70,d,nx,ny,nz)
    call read_3d(70,u,nx,ny,nz)
    close(70)
    print*, 'read data ok'
    !===================== USER DEFINED ======================================



    ! locations = 0 means data in node, 1 means data in cell(not supported yet)
    locations = 0
    ! shared_list(i)=-1 means the i-th data is not shared in this zone. If shared_list(i)=m,
    ! it means the i-th data is shared with zone m in this file
    shared_list = -1
    ! type_list(i) = 1 means the i-th data is of type float. (Other data type not supported yet.)
    type_list = 1

    ! call init subroutine first
    ! nx, ny, nz means the dimension of the data
    ! 'x,y,z,u,v,w' is a string contains names of variables, must be divided by ','
    call plt_file%init(filename,nx,ny,nz,'Tecplot File Title','x,y,z,Q,u')

  
    ! for each zone, call the two subroutines
    ! physics_time can be any value, it will only be used when there are more than 1 zone in a file.
    call plt_file%write_zone_header('zone name', physics_time, 0, locations)

    ! your_datas(:,:,:,1:3) =  x,y,z coordinates(Variable assignment is omitted in this example)
    ! your_datas(:,:,:,4:6) =  u,v,w datas (Variable assignment is omitted in this example)
    ! ALL datas are stored in sequence like (((x(ix,iy,iz),ix=1,nx),iy=1,ny),iz=1,nz)
    ! set coordinate
    your_datas(:,:,:,1) = real(xx3d, 4)
    your_datas(:,:,:,2) = real(yy3d, 4)
    your_datas(:,:,:,3) = real(zz3d, 4)

    ! set value
    !===================== USER DEFINED ======================================
    your_datas(:,:,:,4) = real(Q, 4)
    your_datas(:,:,:,5) = real(u, 4)
    !===================== USER DEFINED ======================================

    
    call plt_file%write_zone_data(type_list, shared_list, your_datas)

    ! before exit, you must call complete subroutine
    call plt_file%complete

    deallocate(your_datas, locations, type_list, shared_list, xx3d, yy3d, zz3d, Q, U, d)
    deallocate(v,w,T)
  end subroutine example1

end program main
