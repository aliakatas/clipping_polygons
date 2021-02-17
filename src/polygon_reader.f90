
    module polygon_reader
    use polygon_data
    use polygon_writer
    
    implicit none
    
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
    subroutine create_polygon(fname, poly, description)
    character(*), intent(in)            :: fname
    type(Polygon), intent(out)          :: poly
    character(*), intent(in), optional  :: description
    
    integer                 :: fid
    type(Vertex), allocatable       :: ppoints(:)
    
    
    open(newunit=fid, file=fname)
    
    ! read polygons
    call read_polyfile(fid, ppoints)
#ifdef _DEBUG    
    if (present(description)) then
        call print_list_of_points(ppoints, description)
    else
        call print_list_of_points(ppoints, fname)
    end if
#endif
    
    call create_polygon_from_vertices(ppoints, poly)
#ifdef _DEBUG
    if (present(description)) then
        call print_polygon(poly, description)
    else
        call print_polygon(poly, fname)
    end if
#endif    
    
    close(fid)
    end subroutine create_polygon
    
    end module polygon_reader
    
    