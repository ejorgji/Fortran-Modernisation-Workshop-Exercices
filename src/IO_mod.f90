!> this module realises In/Out operations
module IO_mod 
  use, intrinsic :: iso_fortran_env
  use :: types_mod, only: dp
  use netcdf 
  implicit none
  ! everything is public unless otherwise stated
  public

contains

  subroutine r8mat_write(output_filename, x, t, hmat)
    implicit none

    !integer :: j
    !> output file name
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    !character (len=30) :: string
    !> heat matrix
    real (kind=dp), dimension(:,:), intent(in) :: hmat
	!> space vector 
    real(kind=dp), dimension(:), intent(in) :: x 
	!> time vector 
    real(kind=dp), dimension(:), intent(in) :: t 

    integer :: m
    integer :: n

	integer :: ierr, ncid, x_dimid, t_dimid, x_varid, t_varid, h_varid 
	
	m = size( hmat(:, :), 1 )
	n = size( hmat(:, :), 2 )
	
	ierr = NF90_CREATE( output_filename, NF90_CLOBBER, ncid ) 
	! entering define mode
	ierr = NF90_DEF_DIM( ncid, "x", m, x_dimid ) 
	ierr = NF90_DEF_DIM( ncid, "t", n, t_dimid )
	ierr = NF90_DEF_VAR( ncid, "x-range", NF90_INT, x_dimid, x_varid )
	ierr = NF90_DEF_VAR( ncid, "t-range", NF90_INT, t_dimid, t_varid )
	ierr = NF90_DEF_VAR( ncid, "solution", NF90_INT, [ x_dimid, t_dimid ], h_varid )
	! put attributes
	ierr = NF90_PUT_ATT( ncid,  NF90_GLOBAL,  "purpose", "Fortran workshop"  ) 
	ierr = NF90_PUT_ATT( ncid,  NF90_GLOBAL, "name", "E Jorgji" ) 
	ierr = NF90_PUT_ATT( ncid,  NF90_GLOBAL, "institution", "CEA" ) 
	ierr = NF90_PUT_ATT( ncid,  x_varid, "dimension", "1D" ) 
	ierr = NF90_PUT_ATT( ncid,  t_varid, "dimension", "1D" ) 
	ierr = NF90_PUT_ATT( ncid,  h_varid, "dimension", "2D" ) 
	ierr = NF90_PUT_ATT( ncid,  x_varid, "unit", "meters" ) 
	ierr = NF90_PUT_ATT( ncid,  t_varid, "unit", "seconds" ) 
	ierr = NF90_PUT_ATT( ncid,  h_varid, "unit", "Celsius" ) 
	ierr = NF90_ENDDEF( ncid ) 
	! end define mode and enter data mode
	ierr = NF90_PUT_VAR( ncid, x_varid, x ) 
	ierr = NF90_PUT_VAR( ncid, t_varid, t ) 
	ierr = NF90_PUT_VAR( ncid, h_varid, hmat ) 
	! write data 
	ierr = NF90_CLOSE( ncid )
	
    !output_unit_id = 10
    !open (unit=output_unit_id, file=output_filename, status='replace')

    !write (string, '(a1,i8,a1,i8,a1,i8,a1)') '(', m, 'g', 24, '.', 16, ')'

    !do j = 1, n
    !  write (output_unit_id, string) hmat(1:m, j)
    !end do

    !close (unit=output_unit_id)
  end subroutine

  !> linear space vector 
  subroutine r8vec_linspace(a_first, a_last, a)

    implicit none
    real (kind=dp), dimension(:), intent(out) :: a
    real (kind=dp), intent(in) :: a_first
    real (kind=dp), intent(in) :: a_last
    integer :: i

    integer :: n
	
	n = size( a(:) )

    do i = 1, n
      a(i) = (real(n-i,kind=dp)*a_first+real(i-1,kind=dp)*a_last)/ &
        real(n-1, kind=dp)
    end do

  end subroutine

  !> write vector 
  subroutine r8vec_write(output_filename, x)

    implicit none

    integer :: j
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    real (kind=dp), dimension(:), intent(in) :: x
    integer :: n
	
	n = size( x(:) )

    output_unit_id = 11
    open (unit=output_unit_id, file=output_filename, status='replace')

    do j = 1, n
      write (output_unit_id, '(2x,g24.16)') x(j)
    end do

    close (unit=output_unit_id)
  end subroutine

  
 
end module IO_mod
