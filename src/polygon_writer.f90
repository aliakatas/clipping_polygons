
    module polygon_writer
    use polygon_data
    
    implicit none
    
    contains
    
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
    subroutine save_polygon_to_file(fname, poly)
    character(*), intent(in)            :: fname
    type(Polygon), intent(in)           :: poly
    
    integer             :: nsides, i
    integer             :: fid
    
    nsides = poly%nsides
    
    open(newunit=fid, file=fname)
    write(fid, '(i0)') nsides
    do i = 1, nsides
        write(fid, '(i0,f12.4,f12.4)') i, poly%edges(i)%start%x, poly%edges(i)%start%y
    end do
    
    close(fid)
    end subroutine save_polygon_to_file
    
    end module polygon_writer
    
    