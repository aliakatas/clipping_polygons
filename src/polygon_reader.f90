
    module polygon_reader
    
    implicit none
    
    type Vertex
        real    :: x
        real    :: y
    end type Vertex
    
    type Edge
        type(Vertex) :: start
        type(Vertex) :: finish
    end type Edge
    
    type Polygon
        integer                 :: nsides
        type(Edge), allocatable :: edges(:)
    end type Polygon
    
    contains
    
    !*********************************************************
    subroutine read_polyfile(fid, points)
    integer, intent(inout)                      :: fid
    type(Vertex), allocatable, intent(inout)    :: points(:)
    
    integer         :: npoints, i
    integer         :: dummy
    
    read(fid, *) npoints
    allocate(points(npoints))
    
    do i = 1, npoints
        read(fid, *) dummy, points(i)%x, points(i)%y
    end do
    end subroutine read_polyfile
    
    !*********************************************************
    subroutine print_list_of_points(points, message)
    type(Vertex), allocatable, intent(in)       :: points(:)
    character(*), intent(in), optional          :: message
    
    integer                     :: i, n
    
    n = size(points, 1)
    
    write(*,'(A)') '---------------------'
    if (present(message)) then
        write(*,'(A)') trim(adjustl(message))
    end if
    
    do i = 1, n
        write(*,'(f8.3,2X,f8.3)') points(i)%x, points(i)%y 
    end do
    
    write(*,'(A)') '---------------------'
    end subroutine print_list_of_points
    
    !*********************************************************
    subroutine create_polygon_from_vertices(points, poly)
    type(Vertex), allocatable, intent(in)       :: points(:)
    type(Polygon), intent(out)                  :: poly
    
    integer                             :: n, i
    type(Edge), allocatable             :: edges
    
    n = size(points, 1)
    
    poly%nsides = n
    allocate(poly%edges(n))
    
    do i = 1, n
        poly%edges(i)%start = points(i)
        poly%edges(i)%finish = points(mod(i, n) + 1)
    end do
    
    end subroutine create_polygon_from_vertices
    
    !*********************************************************
    subroutine print_polygon(poly, message)
    type(Polygon), intent(in)               :: poly
    character(*), intent(in), optional      :: message
    
    integer         :: i
    
    write(*,'(A)') '---------------------'
    if (present(message)) then
        write(*,'(A)') trim(adjustl(message))
    end if
    
    do i = 1, poly%nsides
        write(*,'(i0,a,f8.3,a,f8.3,a,f8.3,a,f8.3,a)') i, ' : (', poly%edges(i)%start%x, ', ', poly%edges(i)%start%y, ')  -->  (',&
             poly%edges(i)%finish%x, ', ', poly%edges(i)%finish%y, ')'
    end do
    
    write(*,'(A)') '---------------------'
    end subroutine print_polygon
    
    end module polygon_reader
    
    