!> this module realises In/Out operations
module IO_mod 
  use, intrinsic :: iso_fortran_env
  use :: types_mod, only: dp
  implicit none
  ! everything is public unless otherwise stated
  public

contains

  subroutine r8mat_write(output_filename, table)
    implicit none

    integer :: j
    !> output file name
    character (len=*), intent(in) :: output_filename
    integer :: output_unit_id
    character (len=30) :: string
    !> heat matrix
    real (kind=dp), dimension(:,:), intent(in) :: table

    integer :: m
    integer :: n

	m = size( table(:, :), 1 )
	n = size( table(:, :), 2 )
	
    output_unit_id = 10
    open (unit=output_unit_id, file=output_filename, status='replace')

    write (string, '(a1,i8,a1,i8,a1,i8,a1)') '(', m, 'g', 24, '.', 16, ')'

    do j = 1, n
      write (output_unit_id, string) table(1:m, j)
    end do

    close (unit=output_unit_id)
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
