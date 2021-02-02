

    program clip_polygon_driver
    use polygon_reader

    implicit none

    integer         :: nargs
    integer         :: fid_clipping, fid_original, fid_result
    character(512)  :: filename_clipping, filename_original, filename_result
    
    type(Vertex), allocatable       :: clipper_points(:), original_points(:), result_points(:)
    type(Polygon)                   :: clipper, original, resulting

    filename_clipping = ' '
    filename_original = ' '
    filename_result = ' '

    write(*,*) ' Attempting to clip polygon...'

    nargs = command_argument_count()
    ! Could consider a more elaborate method to get files (switches maybe?)
    if (nargs > 3) then
        call get_command_argument(1, filename_clipping)
        call get_command_argument(2, filename_original)
        call get_command_argument(2, filename_result)
    else
        filename_clipping = '../data/clipping_polygon.txt'
        filename_original = '../data/convex_polygon.txt'
        !filename_original = '../data/concave_polygon.txt'
        filename_result = '../data/clipped_polygon.txt'
    end if

    open(newunit=fid_clipping, file=trim(adjustl(filename_clipping)))
    open(newunit=fid_original, file=trim(adjustl(filename_original)))
    open(newunit=fid_result, file=trim(adjustl(filename_result)))

    ! read polygons
    call read_polyfile(fid_clipping, clipper_points)
    call read_polyfile(fid_original, original_points)
    call read_polyfile(fid_result, result_points)
    
#ifdef _DEBUG
    call print_list_of_points(clipper_points, 'clipper')
    call print_list_of_points(original_points, 'original')
    call print_list_of_points(result_points, 'result')
#endif    
    
    call create_polygon_from_vertices(clipper_points, clipper)
    call create_polygon_from_vertices(original_points, original)
    call create_polygon_from_vertices(result_points, resulting)
    
#ifdef _DEBUG
    call print_polygon(clipper, 'clipper')
    call print_polygon(original, 'original')
    call print_polygon(resulting, 'result')
#endif 

    ! clip it

    ! write results
    
    

    close(fid_clipping)
    close(fid_original)
    close(fid_result)

    write(*,*) ' All done.'
    write(*,*) ' Goodbye'
    
#ifdef _DEBUG
    read(*,*)
#endif    

    end program clip_polygon_driver