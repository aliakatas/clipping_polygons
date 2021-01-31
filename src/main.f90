
program clip_polygon_driver

    implicit none

    integer         :: nargs
    integer         :: fid_clipping, fid_original, fid_result
    character(512)  :: filename_clipping, filename_original, filename_result

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
        filename_clipping = './data/clipping_polygon.txt'
        filename_original = './data/convex_polygon.txt'
        !filename_original = './data/concave_polygon.txt'
        filename_result = './data/clipped_polygon.txt'
    end if

    open(newunit=fid_clipping, file=trim(adjustl(filename_clipping)))
    open(newunit=fid_original, file=trim(adjustl(filename_original)))
    open(newunit=fid_result, file=trim(adjustl(filename_result)))

    ! read polygons

    ! clip it

    ! write results

    close(fid_clipping)
    close(fid_original)
    close(fid_result)

    write(*,*) ' All done.'
    write(*,*) ' Goodbye'

end program clip_polygon_driver