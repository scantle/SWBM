module m_global
!-------------------------------------------------------------------------------------------------!
! Everything that's needed everywhere gets hidden here
!-------------------------------------------------------------------------------------------------!
  integer          :: nmonths
  integer          :: nrows
  integer          :: ncols
  integer          :: nSFR_inflow_segs
  integer          :: WYstart
  character(100)   :: model_name
  

  
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!  
  subroutine init_globals()
  
  ! Integers
  nmonths          = 0
  nrows            = 0
  ncols            = 0
  nSFR_inflow_segs = 0
  WYstart          = 0
  
  ! Reals
  
  
  ! Characters
  model_name = ""
  
  end subroutine init_globals
  
end module m_global