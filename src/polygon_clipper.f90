
    module polygon_clipper
    use polygon_data
    use polygon_writer
    use polygon_reader, only : create_polygon_from_vertices

    implicit none

    real, parameter         :: NODATA = -99999.9
    real, parameter         :: SMALL_REAL = 100. * epsilon(1.)

    contains

    !*********************************************************
    subroutine clipper(clipping_poly, clipped_poly)
    ! https://en.wikipedia.org/wiki/Sutherland%E2%80%93Hodgman_algorithm
    type(Polygon), intent(in)       :: clipping_poly
    type(Polygon), intent(inout)    :: clipped_poly

    integer             :: clipping_size, clipped_size, nverts
    integer             :: iedge, ivert
    type(Vertex), allocatable   :: vertices(:), vertices_out(:)
    type(Vertex)        :: current_point, previous_point, intersecting_point
    
#ifdef _DEBUG
    character(50)       :: msg
#endif    

#ifdef _DEBUG
    call print_polygon(clipped_poly, 'before clipping')
#endif

    clipping_size = clipping_poly%nsides
    clipped_size = clipped_poly%nsides

#ifdef _DEBUG
    write(*,*) 'Number of edges of clipping polygon :: ', clipping_size
    write(*,*) '    Number of edges before clipping :: ', clipped_size
#endif

    vertices = get_vertices_from_polygon(clipped_poly)
    allocate(vertices_out, source = vertices)

    do iedge = 1, clipping_size
        deallocate(vertices)
        allocate(vertices, source = vertices_out)
        
#ifdef _DEBUG
        write(msg, '(a,x,i0)') 'Iteration', iedge
        call print_list_of_points(vertices_out, msg)
#endif        
        deallocate(vertices_out)
        nverts = size(vertices, 1)

        do ivert = 1, nverts
            current_point = vertices(mod(ivert, nverts) + 1)
            previous_point = vertices(ivert)

            intersecting_point = find_intersection(previous_point, current_point, clipping_poly%edges(iedge))

            if (vertex_inside_polygon(current_point, clipping_poly%edges(iedge))) then
                if (.not. vertex_inside_polygon(previous_point, clipping_poly%edges(iedge))) then
                    if (intersecting_point%x > NODATA .and. intersecting_point%y > NODATA) then
                        call append_to_list_of_vertices(vertices_out, intersecting_point)
                    end if
                end if
                call append_to_list_of_vertices(vertices_out, current_point)
            else if (vertex_inside_polygon(previous_point, clipping_poly%edges(iedge))) then

                if (intersecting_point%x > NODATA .and. intersecting_point%y > NODATA) then
                    call append_to_list_of_vertices(vertices_out, intersecting_point)
                end if
            end if
        end do
    end do
    
    call create_polygon_from_vertices(vertices_out, clipped_poly)
#ifdef _DEBUG
    call print_polygon(clipped_poly, 'after clipping')
#endif
    end subroutine clipper

    !*********************************************************
    function find_intersection(p1, p2, clip_edge) result(int_point)
    ! https://en.wikipedia.org/wiki/Line%E2%80%93line_intersection
    type(Vertex), intent(in)            :: p1, p2
    type(Edge), intent(in)              :: clip_edge
    type(Vertex)                        :: int_point
    
    real                :: crossproduct 
    real                :: t, u
    real                :: x12, y12, x34, y34
    
    crossproduct = (p2%x - p1%x) * (clip_edge%finish%y - clip_edge%start%y) - (p2%y - p1%y) * (clip_edge%finish%x - clip_edge%start%x)
    
    if (abs(crossproduct) > SMALL_REAL) then
        x12 = p1%x - p2%x
        y12 = p1%y - p2%y
        x34 = clip_edge%start%x - clip_edge%finish%x
        y34 = clip_edge%start%y - clip_edge%finish%y
            
        t = ((p1%x - clip_edge%start%x) * y34 - (p1%y - clip_edge%start%y) * x34) / (x12 * y34 - y12 * x34)
        u = ((-x12) * (p1%y - clip_edge%start%y) - (-y12) * (p1%x - clip_edge%start%x)) / (x12 * y34 - y12 * x34)
        
        ! No need to check it's falling in both line segments, original edge will do
        if (t >= 0.0 .and. t <= 1.0) then
            int_point%x = p1%x + t * (p2%x - p1%x)
            int_point%y = p1%y + t * (p2%y - p1%y)
            return
        end if
    end if
    
    ! Handle parallel line segments AND point of intersection outside of both line segments
    int_point%x = NODATA
    int_point%y = NODATA
    end function find_intersection

    !*********************************************************
    function vertex_on_edge(vert, clip_edge) result(ok)
    type(Vertex), intent(in)        :: vert
    type(Edge), intent(in)          :: clip_edge
    logical                         :: ok

    real                :: crossproduct, dotproduct, sqlength

    ok = .false.

    crossproduct = (vert%y - clip_edge%start%y) * (clip_edge%finish%x - clip_edge%start%x) - (vert%x - clip_edge%start%x) * (clip_edge%finish%y - vert%y)
    if (crossproduct > epsilon(1.0)) return

    dotproduct = (vert%x - clip_edge%start%x) * (clip_edge%finish%x - clip_edge%start%x) + (vert%y - clip_edge%start%y) * (clip_edge%finish%y - clip_edge%start%y)
    if (dotproduct < 0.0) return

    sqlength = (clip_edge%finish%x - clip_edge%start%x) * (clip_edge%finish%x - clip_edge%start%x) + (clip_edge%finish%y - clip_edge%start%y) * (clip_edge%finish%y - clip_edge%start%y)
    if (dotproduct > sqlength) return

    ok = .true.
    end function vertex_on_edge

    !*********************************************************
    function vertex_inside_polygon(vert, clip_edge) result(ok)
    type(Vertex), intent(in)        :: vert
    type(Edge), intent(in)          :: clip_edge
    logical                         :: ok

    real                :: crossproduct
    
    ! Assumes counter-clockwise definition of points
    crossproduct = (clip_edge%finish%x - clip_edge%start%x) * (vert%y - clip_edge%start%y) - (clip_edge%finish%y - clip_edge%start%y) * (vert%x - clip_edge%start%x)
    ok = crossproduct >= 0.0
    end function vertex_inside_polygon

    end module polygon_clipper

