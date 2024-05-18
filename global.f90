module m_global
!-------------------------------------------------------------------------------------------------!
! Everything that's needed everywhere gets hidden here
!-------------------------------------------------------------------------------------------------!
  integer          :: nmonths
  integer          :: nrows
  integer          :: ncols
  integer          :: nSFR_inflow_segs
  integer          :: WYstart
  integer          :: n_daily_out
  character(100)   :: model_name
  
  ! Required Files
  character(100)   :: precip_file
  character(100)   :: et_file
  character(100)   :: et_ext_depth_file
  character(100)   :: recharge_zones_file
  character(100)   :: kc_frac_file
  character(100)   :: sfr_partition_file
  character(100)   :: poly_landcover_file
  
  ! Optional Files
  character(100)   :: ditch_file
  character(100)   :: et_zones_file
  character(100)   :: MAR_depth_file
  character(100)   :: curtail_frac_file
  
  ! Output arrays
  integer,allocatable       :: daily_out_idx(:)
  character(50),allocatable :: daily_out_nms(:)
  
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
  n_daily_out      = 0
  
  ! Reals
  
  
  ! Characters
  model_name          = ""
  precip_file         = ""
  et_file             = ""
  et_ext_depth_file   = ""
  kc_frac_file        = ""
  sfr_partition_file  = ""
  poly_landcover_file = ""
  ditch_file          = ""
  et_zones_file       = ""
  MAR_depth_file      = ""
  curtail_frac_file   = ""
  recharge_zones_file = ""
  
  end subroutine init_globals
  
end module m_global