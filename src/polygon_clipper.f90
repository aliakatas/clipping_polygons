
    module polygon_clipper
    use polygon_data
    use polygon_writer
    
    implicit none
    
    real, parameter         :: NODATA = -99999.9
    
    contains 
    
    !*********************************************************
    subroutine clipper(clipping_poly, clipped_poly)
    type(Polygon), intent(in)       :: clipping_poly
    type(Polygon), intent(inout)    :: clipped_poly
    
    integer             :: clipping_size, clipped_size, nverts
    integer             :: iedge, ivert, ip1
    type(Vertex), allocatable   :: vertices(:), vertices_out(:)
    type(Vertex)        :: current_point, previous_point, intersecting_point
    
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
        vertices = vertices_out
        deallocate(vertices_out)
        nverts = size(vertices, 1)
        
        do ivert = 1, nverts
            ip1 = ivert + 1
            if (ip1 > nverts) ip1 = 1
            
            current_point = vertices(ip1) 
            previous_point = vertices(ivert)
            
            intersecting_point = find_intersection(previous_point, current_point, clipping_poly%edges(iedge))
            
            if (vertex_in_edge(current_point, clipping_poly%edges(iedge))) then
                if (.not. vertex_in_edge(previous_point, clipping_poly%edges(iedge))) then
                    if (intersecting_point%x > NODATA .and. intersecting_point%y > NODATA) then
                        call append_to_list_of_vertices(vertices_out, intersecting_point)
                    end if  
                end if
                call append_to_list_of_vertices(vertices_out, current_point)
            else if (vertex_in_edge(previous_point, clipping_poly%edges(iedge))) then
                
                if (intersecting_point%x > NODATA .and. intersecting_point%y > NODATA) then
                    call append_to_list_of_vertices(vertices_out, intersecting_point)
                end if
            end if
        end do
    end do
    
    end subroutine clipper
    
    !*********************************************************
    function find_intersection(p1, p2, clip_edge) result(int_point)
    type(Vertex), intent(in)            :: p1, p2
    type(Edge), intent(in)              :: clip_edge
    type(Vertex)                        :: int_point
    
    real                :: s1_x, s1_y, s2_x, s2_y
    real                :: s, t
    
    s1_x = p2%x - p1%x
    s1_y = p2%y - p1%y
    
    s2_x = clip_edge%finish%x - clip_edge%start%x
    s2_y = clip_edge%finish%y - clip_edge%start%y;

    s = (-s1_y * (p1%x - clip_edge%finish%x) + s1_x * (p1%y - clip_edge%finish%y)) / (-s2_x * s1_y + s1_x * s2_y);
    t = ( s2_x * (p1%y - clip_edge%finish%y) - s2_y * (p1%x - clip_edge%finish%x)) / (-s2_x * s1_y + s1_x * s2_y);

    if (s >= 0. .and. s <= 1. .and. t >= 0. .and. t <= 1.) then
        int_point%x = p1%x + (t * s1_x)
        int_point%y = p1%y + (t * s1_y)
        return
    end if
    
    int_point%x = NODATA
    int_point%y = NODATA
    end function find_intersection
    
    !*********************************************************
    function vertex_in_edge(vert, clip_edge) result(ok)
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
    end function vertex_in_edge
    
    end module polygon_clipper
    
    