

    program clip_polygon_driver
    use polygon_data, only  : Polygon
    use polygon_reader, only    : create_polygon
    use polygon_clipper, only   : clipper
    use polygon_writer, only    : save_polygon_to_file

    implicit none

    integer         :: nargs
    character(512)  :: filename_clipping, filename_original, filename_result
    
    type(Polygon)   :: clipper_poly, original_poly, resulting_poly

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
    
    ! create polygons
    call create_polygon(trim(adjustl(filename_clipping)), clipper_poly, 'clipper polygon')
    call create_polygon(trim(adjustl(filename_original)), original_poly, 'original polygon')
    
    ! create a copy of the original polygon
    resulting_poly = original_poly
    
    ! clip it
    call clipper(clipper_poly, resulting_poly)

    ! write results
    call save_polygon_to_file(filename_result, resulting_poly)
    
    write(*,*) ' All done.'
    write(*,*) ' Goodbye'
    
#ifdef _DEBUG
    read(*,*)
#endif    

    end program clip_polygon_driver