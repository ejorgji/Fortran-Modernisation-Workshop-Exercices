!> this module provides the Solver
module Solver_mod 
  use, intrinsic :: iso_fortran_env
  use :: types_mod, only: dp
  use :: rhs_mod
  implicit none
  ! everything is public unless otherwise stated
  public

contains

  !> Solver subroutine
  subroutine fd1d_heat_explicit(x, t, dt, cfl, h, h_new)
    implicit none

    !> calculated CFL number 
    real (kind=dp), intent(in) :: cfl
    real (kind=dp), intent(in) :: dt
    !> input heat data 
    real (kind=dp), dimension(:), intent(in) :: h
    !> output heat data 
    rreal (kind=dp), dimension(:), intent(out) :: h_new
    integer :: j
    real (kind=dp), intent(in) :: t
    real (kind=dp), dimension(:), intent(in) :: x
    real (kind=dp), dimension(:) :: f(size(x))

    do j = 1, size(x)
      f(j) = func(j, x)
    end do

    h_new(1) = 0.0e+00_dp

    do j = 2, size(x) - 1
      h_new(j) = h(j) + dt*f(j) + cfl*(h(j-1)-2.0e+00_dp*h(j)+h(j+1))
    end do

! set the boundary conditions again
    h_new(1) = 90.0e+00_dp
    h_new(size(x)) = 70.0e+00_dp
  end subroutine

 
end module Solver_mod
