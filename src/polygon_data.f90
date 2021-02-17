
    module polygon_data
    
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
    function get_vertices_from_polygon(poly) result(points)
    type(Polygon), intent(in)   :: poly
    type(Vertex), allocatable   :: points(:)
    
    integer         :: n, i
    
    n = poly%nsides
    allocate(points(n))
    
    do i = 1, n
        points(i)%x = poly%edges(i)%start%x
        points(i)%y = poly%edges(i)%start%y
    end do
    end function get_vertices_from_polygon
    
    !*********************************************************
    subroutine append_to_list_of_vertices(vertices, vert)
    type(Vertex), allocatable, intent(inout)    :: vertices(:)
    type(Vertex), intent(in)                    :: vert
    
    type(Vertex), allocatable               :: temp_vertices(:)
    integer                                 :: n
    
    if (allocated(vertices)) then
        n = size(vertices, 1)
        call move_alloc(vertices, temp_vertices)
        
        allocate(vertices(n + 1))
        vertices(1:n) = temp_vertices(1:n)
        vertices(n + 1) = vert
        deallocate(temp_vertices)
    else
        allocate(vertices(1))
        vertices(1) = vert
    end if
    end subroutine append_to_list_of_vertices
    
    end module polygon_data
    