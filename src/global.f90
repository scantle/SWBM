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
  character(100)   :: poly_landcover_file
  character(100)   :: poly_agwell_file
  character(100)   :: sfr_template_file
  character(100)   :: ets_template_file
  character(100)   :: wel_template_file
  character(100)   :: agwell_locs_file
  character(100)   :: sfr_routing_file
  character(100)   :: sfr_segments_file
  character(100)   :: irr_inflows_file
  character(100)   :: non_irr_inflows_file
  
  ! Optional Files
  character(100)   :: ditch_file
  character(100)   :: et_zones_file
  character(100)   :: et_cor_file
  character(100)   :: MAR_depth_file
  character(100)   :: curtail_frac_file
  character(100)   :: water_mover_file
  character(100)   :: sfr_jtf_file
  character(100)   :: specwell_locs_file
  character(100)   :: specwell_vol_file
  character(100)   :: mfr_wells_file
  character(100)   :: mfr_catchment_vols_file
  character(100)   :: mfr_catchment_mult_file
  character(100)   :: sfr_diversions_file
  
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
  model_name              = ""
  precip_file             = ""
  et_file                 = ""
  et_ext_depth_file       = ""
  kc_frac_file            = ""
  poly_landcover_file     = ""
  sfr_template_file       = ""
  ets_template_file       = ""
  wel_template_file       = ""
  agwell_locs_file        = ""
  sfr_routing_file        = ""
  sfr_segments_file       = ""
  irr_inflows_file        = ""
  non_irr_inflows_file    = ""
  ditch_file              = ""
  et_zones_file           = ""
  et_cor_file             = ""
  MAR_depth_file          = ""
  curtail_frac_file       = ""
  recharge_zones_file     = ""
  water_mover_file        = ""
  sfr_jtf_file            = ""
  specwell_locs_file      = ""
  specwell_vol_file       = ""
  mfr_wells_file          = ""
  mfr_catchment_vols_file = ""
  mfr_catchment_mult_file = ""
  sfr_diversions_file     = ""
  
  end subroutine init_globals
  
end module m_global