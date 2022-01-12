MODULE define_fields

IMPLICIT NONE

TYPE polygon
  INTEGER   :: SWBM_id, SWBM_LU, subws_ID, irr_type, well_id, well_idx, water_source
  INTEGER   :: nModelCells, landcover_id, irr_flag, WL2CP_year, runoff_ISEG
  REAL      :: area, whc, init_fill_frac, max_infil_rate, precip_fact
  LOGICAL   :: Irrigating, ILR, ILR_Active
END TYPE

TYPE accumulator
	INTEGER :: daydef, ET_active
	REAL :: tot_irr, gw_irr, recharge, swc, pET, aET, deficiency, change_in_storage
  REAL :: effprecip, MAR, residual, tot_irr_vol, recharge_vol, gw_irr_vol, swc_vol
	REAL :: pET_vol, aET_vol, deficiency_vol, change_in_storage_vol, effprecip_vol, MAR_vol
	REAL :: runoff, runoff_vol
END TYPE

TYPE well
    INTEGER :: well_id, layer, well_row, well_col
    REAL :: coordx, coordy, top_scrn_z, bot_scrn_z
    REAL :: daily_vol, monthly_vol, annual_vol, monthly_rate, specified_volume, specified_rate
    CHARACTER(50) :: well_name
END TYPE
    
TYPE crop_table
    INTEGER :: landcover_id, IrrMonthStart, IrrDayStart, IrrMonthEnd, IrrDayEnd
    REAL    :: IrrSWC, RootDepth, irreff_flood, irreff_wl, irreff_cp, daily_Kc, Kc_mult
    CHARACTER(50) :: landcover_name
    LOGICAL :: irrigated, ET
END TYPE

TYPE surface_water
    REAL :: inflow_irr, inflow_nonirr, sw_irr, avail_sw_vol
END TYPE

TYPE subws_flow_partitioning
    INTEGER :: subws_ID, SFR_segment
    REAL :: frac_subws_flow
    CHARACTER(50) :: subwsName, streamName
END TYPE

TYPE Stream_Segments
    INTEGER :: NSEG, ICALC, OUTSEG, IUPSEG, IPRIOR
    REAL :: FLOW, WIDTH1, WIDTH2, MANNING_N, RUNOFF 
    CHARACTER(12) :: Bed_K_Param, Manning_n_Param
END TYPE
    
TYPE(polygon), ALLOCATABLE, DIMENSION(:) :: fields
TYPE(accumulator), ALLOCATABLE, DIMENSION(:):: previous, monthly, daily, yearly
TYPE(well), ALLOCATABLE, DIMENSION(:) :: ag_wells, muni_wells
TYPE(crop_table), ALLOCATABLE, DIMENSION(:) :: crops
TYPE(surface_water), ALLOCATABLE, DIMENSION(:) :: surfaceWater
TYPE(subws_flow_partitioning), ALLOCATABLE, DIMENSION(:) :: SFR_allocation
TYPE(Stream_Segments), ALLOCATABLE, DIMENSION(:) :: SFR_Routing
INTEGER :: npoly, nrotations, nAgWells, nMuniWells, ip, nlandcover
REAL :: RD_Mult, ann_spec_ag_vol, ann_spec_muni_vol

contains

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE readpoly(npoly, nrows, ncols, rch_zones)

  INTEGER, INTENT(IN):: npoly, nrows, ncols
  INTEGER, DIMENSION(nrows, ncols), INTENT(IN)  :: rch_zones
  INTEGER :: i, dummy
  INTEGER, DIMENSION(nrows, ncols)  :: dummy_mat
  

  ALLOCATE(fields(npoly), monthly(npoly), daily(npoly), yearly(npoly), previous(npoly))
  daily%tot_irr = 0.
  daily%daydef = 0

  open(unit=10,file="polygons_table.txt",status="old")
  read(10,*)       ! read headers into nothing
  write(800,*)'SWBM_ID subws_ID SWBM_LU SWBM_IRR area &
    &Water_Src whc init_fill_frac WL2CP_year ILR' 
  open(unit=11,file="precip_factors.txt",status="old")
  read(11,*) ! read headers into nothing
   
    do i=1, npoly
      read(10,*)fields(i)%SWBM_id, fields(i)%subws_ID, fields(i)%SWBM_LU, &
        fields(i)%irr_type, fields(i)%area, fields(i)%water_source, fields(i)%whc, &
        fields(i)%init_fill_frac, fields(i)%max_infil_rate, fields(i)%runoff_ISEG,&
        fields(i)%WL2CP_year, fields(i)%ILR
      read(11,*)dummy, fields(i)%precip_fact
      dummy_mat = 0
      where (rch_zones(:,:) == i) 
        dummy_mat(:,:) = 1
      end where
      fields(i)%nModelCells = SUM(dummy_mat)
      write(800,'(i6,i3,i4,i5,f20.7,i5,f20.5,f20.5,i6,l3)')i ,fields(i)%subws_ID, fields(i)%SWBM_LU, &
      fields(i)%irr_type, fields(i)%area, fields(i)%water_source, fields(i)%whc, fields(i)%init_fill_frac, &
      fields(i)%WL2CP_year, fields(i)%ILR
    enddo
  close(10)
  close(11)
end subroutine readpoly

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE read_landcover_table(nlandcover)

  INTEGER, INTENT(IN) :: nlandcover
  INTEGER :: i
 
  ALLOCATE(crops(nlandcover))
  
  open(unit=19, file = 'landcover_table.txt', status = 'old')
  read(19,*) ! read header into nothing
  do i=1, nlandcover
    read(19,*) crops(i)%landcover_id, crops(i)%landcover_name, crops(i)%irrigated, crops(i)%ET, &
    crops(i)%IrrSWC, crops(i)%IrrMonthStart, crops(i)%IrrDayStart, crops(i)%IrrMonthEnd, crops(i)%IrrDayEnd, &
    crops(i)%RootDepth, crops(i)%irreff_flood, crops(i)%irreff_wl, crops(i)%irreff_cp, crops(i)%kc_mult
  enddo
  
END SUBROUTINE read_landcover_table

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE initialize_wells(npoly, nAgWells, nMuniWells)                                                                           
                                                                                               
  INTEGER, INTENT(IN) :: npoly, nAgWells, nMuniWells
  INTEGER, DIMENSION(nAgWells) :: ag_well_id
  INTEGER, DIMENSION(nMuniWells) :: muni_well_id
  INTEGER :: i, j, poly_id, well_id
  LOGICAL :: ag_wells_specified
  CHARACTER(20) :: dummy
                                                                                                 
  ALLOCATE(ag_wells(nAgWells))
  ALLOCATE(muni_wells(nMuniWells))
  
  open(unit=537, file="ag_well_specified_volume.txt", status="old")                                  
  read(537,*)  dummy, dummy, ag_well_id(:)    ! read well_id for ag wells
  open(unit=10, file="ag_well_summary.txt", status="old")                                 
  read(10,*)  
  do i=1, nAgWells                                                                          
    read(10,*) ag_wells(i)%well_id, ag_wells(i)%well_name, ag_wells(i)%top_scrn_z, ag_wells(i)%bot_scrn_z,&
    ag_wells(i)%well_row, ag_wells(i)%well_col, ag_wells(i)%coordx, ag_wells(i)%coordy
    ag_wells(i)%well_name = trim(ag_wells(i)%well_name)
    if (ag_wells(i)%well_id /= ag_well_id(i)) then
    	write(*,*) "Ordering of agricultural wells in ag_well_summary.txt differes from that in ag_well_pumping_rates.txt"
    	write(800,*) "Ordering of agricultural wells in ag_well_summary.txt differes from that in ag_well_pumping_rates.txt"
    call EXIT
    endif
  enddo 
  close(10)
  fields%well_id = 0 
  open(unit=10, file="ag_well_list_by_polygon.txt", status="old")                                                                                                                                                                                                                         
  read(10,*)  ! read header into nothing
  write(800,*)'SWBM_ID  Well_ID  WELL_IDX'
  do i = 1, npoly                                                                                                             
    read(10,*) poly_id, well_id                                                                     
    do j = 1, nAgWells                                                                      
      if (poly_id==fields(i)%SWBM_id) fields(i)%well_id = well_id
      if (well_id==ag_wells(j)%well_id) fields(i)%well_idx = j                             
    enddo
  write(800,'(3i6)') fields(i)%SWBM_id, fields(i)%well_id, fields(i)%well_idx 
  enddo
  close(10)
  open(unit=10, file="muni_well_summary.txt", status="old")                                 
  read(10,*)                                                 
  open(unit=539, file="muni_well_specified_volume.txt", status="old")                                 
  read(539,*)  dummy, muni_well_id(:)                                             
  do i=1, nMuniWells                                                                          
    read(10,*) muni_wells(i)%well_id, muni_wells(i)%well_name, muni_wells(i)%top_scrn_z, muni_wells(i)%bot_scrn_z,&
    muni_wells(i)%well_row, muni_wells(i)%well_col, muni_wells(i)%coordx, muni_wells(i)%coordy
    muni_wells(i)%well_name = trim(muni_wells(i)%well_name) 
    if (muni_wells(i)%well_id /= muni_well_id(i)) then
    	write(*,*) "Ordering of municpal wells in muni_well_summary.txt differes from that in muni_well_pumping_rates.txt"
    	write(800,*) "Ordering of municpal wells in muni_well_summary.txt differes from that in muni_well_pumping_rates.txt"
    call EXIT
    endif
  enddo 
  close(10)                                                                                          
END subroutine initialize_wells

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_month

  monthly%tot_irr = 0.         
  monthly%recharge = 0.               
  monthly%gw_irr = 0.           
  monthly%swc = 0.           
  monthly%pET = 0.         
  monthly%aET = 0.          
  monthly%deficiency = 0.      
  monthly%effprecip = 0.          
  monthly%change_in_storage = 0.          
  monthly%MAR = 0.  
  monthly%MAR_vol = 0.
  monthly%runoff = 0.            
    
end subroutine zero_month
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_year
    
  yearly%tot_irr = 0.
  yearly%recharge = 0.
  yearly%gw_irr = 0.
  yearly%swc = 0.
  yearly%pET = 0.
  yearly%aET = 0.
  yearly%deficiency = 0. 
  yearly%effprecip = 0. 
  yearly%change_in_storage = 0.
  yearly%MAR = 0.
  yearly%MAR_vol = 0.
  yearly%runoff = 0.
  
  ann_spec_ag_vol = 0.
  ann_spec_muni_vol = 0.
    
end subroutine zero_year
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
subroutine initial_conditions

  INTEGER :: i

  previous%tot_irr = 0.
  previous%recharge = 0.
  previous%gw_irr = 0.
  previous%pET = 0.
  previous%aET = 0.
  previous%deficiency = 0.
  previous%effprecip = 0.
  previous%change_in_storage = 0.
    
  do i=1, npoly
    daily(i)%swc  = fields(i)%whc*crops(fields(i)%landcover_id)%RootDepth*fields(i)%init_fill_frac
  enddo 
  previous%swc = daily%swc     ! Set previous day's swc to same as initial condition
      
end subroutine initial_conditions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
subroutine IRR2CP(WY)
  
  INTEGER, INTENT(IN) :: WY
  INTEGER :: ip
  
  do ip = 1, npoly
     if (fields(ip)%WL2CP_year == WY) then    ! If WL2CP Year    
       fields(ip)%irr_type = 3                                                       ! Change irrigation type to center pivot
     end if
   enddo
   
end subroutine IRR2CP  
   
end MODULE
