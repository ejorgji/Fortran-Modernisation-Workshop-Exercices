!> this module calculates the RHS number
module RHS_mod 
  use, intrinsic :: iso_fortran_env
  use :: types_mod, only: dp
  implicit none
  ! everything is public unless otherwise stated
  public

contains

  !> calculates the RHS number
  function func(j, x) result (d)
    implicit none

    integer, intent(in) :: j
    real (kind=dp) :: d
    real (kind=dp), dimension(:), intent(in) :: x

    d = 0.0e+00_dp
  end function
 
end module RHS_mod
