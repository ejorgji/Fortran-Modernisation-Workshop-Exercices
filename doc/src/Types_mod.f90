!> this module provides the Types
module Types_mod 
  use, intrinsic :: iso_fortran_env
  implicit none
  ! everything is private unless otherwise stated
  private
  public :: SP, DP
  
  integer, parameter :: SP = REAL32
  integer, parameter :: DP = REAL64
  integer, parameter :: SI = INT32
  integer, parameter :: DI = INT64 
 
end module Types_mod
