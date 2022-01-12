MODULE irrigation
  
  use define_fields
  IMPLICIT NONE
  
  REAL:: kc_grain, kc_alfalfa,kc_alfalfa_mult, kc_grain_mult, kc_noirr
  REAL:: kc_pasture, kc_pasture_mult
  REAL:: irreff_flood, irreff_wl_LU11, irreff_cp_LU11, irreff_wl_LU2, irreff_cp_LU2
  REAL :: AV_REF_ET_1a, AV_REF_ET_1b, AV_REF_ET_2, ETo
  REAL :: monthly_precip_vol
  REAL :: EF_SF_Ratio, Sugar_Ratio, Johnson_Ratio, Crystal_Ratio, Patterson_Ratio
  LOGICAL :: irrigating
  INTEGER  :: nSegs, nSubws, nSFRdiv
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: div_segs, div_IPRIOR
  REAL, ALLOCATABLE, DIMENSION(:)  :: div_rate
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) ::  seg_name 
  
  CONTAINS

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE initialize_streams(nSubws, nSFR_inflow_segs)
  
    INTEGER, INTENT(IN) :: nSubws, nSFR_inflow_segs
    INTEGER :: dummy, i 
    
    open(unit=10, file='SFR_network.txt', status='old')
    DO i=1,6   ! read first 6 comments lines of SFR file into nothing
      read(10,*) 
    ENDDO
    read(10,*) dummy, nSegs
    close(10)
    ALLOCATE(SFR_Routing(nSegs)) ! Allocate arrays of length equal to number of SFR segments       
    ALLOCATE(SFR_allocation(nSFR_inflow_segs))                       ! Allocate array of length equal to number of segments where inflow is specified
    ALLOCATE(surfaceWater(nSubws))                                   ! Allocate array of length equal to number of subwatersheds   
    open(unit=10, file='SFR_routing.txt', status='old')
    read(10,*) ! Read header into nothing
    DO i=1, nSegs
      read(10,*)SFR_Routing(i)%NSEG, SFR_Routing(i)%ICALC, SFR_Routing(i)%OUTSEG, SFR_Routing(i)%IUPSEG, &
                SFR_Routing(i)%IPRIOR, SFR_Routing(i)%WIDTH1, SFR_Routing(i)%WIDTH2, SFR_Routing(i)%MANNING_N, &
                SFR_Routing(i)%Bed_K_Param, SFR_Routing(i)%Manning_n_Param
      SFR_Routing(i)%Bed_K_Param = trim(SFR_Routing(i)%Bed_K_Param)
      SFR_Routing(i)%Manning_n_Param = trim(SFR_Routing(i)%Manning_n_Param)
    ENDDO
    close(10)     
    open(unit=10, file='SFR_inflow_segments.txt', status='old') 
    read(10,*)  ! read header into nothing
    DO i=1, nSFR_inflow_segs
  	  read(10,*) SFR_allocation(i)%subws_ID, SFR_allocation(i)%SFR_segment,&
  	  SFR_allocation(i)%subwsName, SFR_allocation(i)%streamName
    ENDDO
    close(10)
    open(unit = 215, file = 'subwatershed_irrigation_inflows.txt', status = 'old')   
    read(215,*) ! Read header into nothing
    open(unit = 216, file = 'subwatershed_nonirrigation_inflows.txt', status = 'old')   
    read(216,*) ! Read header into nothing
    open(unit = 217, file = 'SFR_diversions.txt', status = 'old')
    read(217,*) nSFRdiv
    ALLOCATE(div_segs(nSFRdiv))
    ALLOCATE(div_rate(nSFRdiv))
    ALLOCATE(div_IPRIOR(nSFRdiv))   
    read(217,*) (div_segs(i), i=1, nSFRdiv)   
    read(217,*) (div_IPRIOR(i), i=1, nSFRdiv) 
    read(217,*) (div_rate(i), i=1, nSFRdiv)   
    
  END SUBROUTINE initialize_streams

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine read_monthly_stream_inflow(inflow_is_vol, numdays)
    
    INTEGER, INTENT(IN) :: numdays
    LOGICAL, INTENT(IN) :: inflow_is_vol
    INTEGER :: i
    CHARACTER(10) :: date_dummy
    
    surfaceWater(:)%inflow_irr = 0.                                                              ! Reset all segment inflows to zero
    surfaceWater(:)%sw_irr = 0.                                                                  ! Reset surface-water irrigation to zero        
    read(215,*) date_dummy, (surfaceWater(i)%inflow_irr, i=1, nSubws)                            ! Read in date and irrigation inflow for each stream simulated
    read(216,*) date_dummy, (surfaceWater(i)%inflow_nonirr, i=1, nSubws)                         ! Read in date and irrigation inflow for each stream simulated
    if (inflow_is_vol) then
    	surfaceWater%avail_sw_vol = surfaceWater%inflow_irr
    else 
      surfaceWater%inflow_irr = surfaceWater%inflow_irr * numdays                                ! If inflow is an average monthly rate, convert to volume
      surfaceWater%inflow_nonirr = surfaceWater%inflow_nonirr * numdays
      surfaceWater%avail_sw_vol = surfaceWater%inflow_irr * numdays           
    end if
  end subroutine read_monthly_stream_inflow
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  subroutine SFR_streamflow(npoly,numdays, nSubws, nSegs, nSFR_inflow_segs)
    
    INTEGER, INTENT(IN) :: npoly, numdays, nSubws, nSegs, nSFR_inflow_segs
    INTEGER :: i, temp_seg, temp_subws_ID
    REAL :: temp_flow
    
    SFR_Routing%FLOW = 0.                                          ! Reset all SFR flow to zero   
    DO i=1,nSFR_inflow_segs
      temp_subws_ID = SFR_allocation(i)%subws_ID
      temp_seg = SFR_allocation(i)%SFR_segment       
      temp_flow = (surfaceWater(temp_subws_ID)%avail_sw_vol + &
      surfaceWater(temp_subws_ID)%inflow_nonirr)  &
      / numdays * SFR_allocation(i)%frac_subws_flow
      SFR_Routing(temp_seg)%FLOW = temp_flow
      
    ENDDO
    
    SFR_Routing(div_segs)%FLOW = div_rate
      
    SFR_Routing%RUNOFF = 0.                                        ! Reset runoff to zero 
    DO i=1,npoly                                                   ! Sum runoff volumes for each SFR segment
      SFR_Routing(fields(i)%runoff_ISEG)%RUNOFF = SFR_Routing(fields(i)%runoff_ISEG)%RUNOFF + &
      monthly(i)%runoff*fields(i)%area 	
    ENDDO 
    SFR_Routing%RUNOFF = SFR_Routing%RUNOFF / numdays              ! Calculate average daily runoff for model input
        
  end subroutine SFR_streamflow
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE IRRIGATION_RULESET(ip, month, jday)
  
    INTEGER, INTENT(IN) :: ip, month, jday
    INTEGER :: subws
    REAL :: irreff   
          
    subws = fields(ip)%subws_ID
    irreff = 0.
    select case (fields(ip)%irr_type)
  	  case(1)   ! Flood irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_flood
  	  case(2)   ! Wheel line irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_wl
  	  case(3)   ! Center pivot irrigation
  	  	irreff = crops(fields(ip)%landcover_id)%irreff_cp
    end select
    if ( (crops(fields(ip)%landcover_id)%irrigated .and. fields(ip)%irr_type /= 555) &                                         ! If field is irrigated
    .and. ((month == crops(fields(ip)%landcover_id)%IrrMonthStart .and. jday >= crops(fields(ip)%landcover_id)%IrrDayStart)&   ! and during defined irrigation season
    .or.  (month > crops(fields(ip)%landcover_id)%IrrMonthStart .and. month < crops(fields(ip)%landcover_id)%IrrMonthEnd)&
    .or.  (month == crops(fields(ip)%landcover_id)%IrrMonthEnd .and. jday <= crops(fields(ip)%landcover_id)%IrrDayEnd))& 
    .and. daily(ip)%swc < crops(fields(ip)%landcover_id)%IrrSWC*fields(ip)%whc*crops(fields(ip)%landcover_id)%RootDepth) then                                           ! and SWC has dropped below defined irrigation trigger   	
      daily(ip)%tot_irr=max(0.,((daily(ip)%pET-daily(ip)%effprecip)/irreff))                                                 ! Calculate applied linear irrigation 	 	
    	select case (fields(ip)%water_source)                                                                                  ! Assign irrigation to water source
    		case(1) ! surface water
    			if (surfaceWater(subws)%avail_sw_vol >= (daily(ip)%tot_irr * fields(ip)%area)) then                                ! If available surface water exceeds demand
    			  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + (daily(ip)%tot_irr * fields(ip)%area)                  ! Add daily irrigation volume to total surface water irrigation 
    			  surfaceWater(subws)%avail_sw_vol = surfaceWater(subws)%avail_sw_vol - (daily(ip)%tot_irr * fields(ip)%area)      ! Update available surface water volume	
    			else if (surfaceWater(subws)%avail_sw_vol < (daily(ip)%tot_irr * fields(ip)%area) &                                  ! If supply is less than demand but greater than 0
    				 .and. surfaceWater(subws)%avail_sw_vol /= 0) then
    				 	daily(ip)%tot_irr = surfaceWater(subws)%avail_sw_vol / fields(ip)%area                                         ! Use remaining surface water for irrigation
            	surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + surfaceWater(subws)%avail_sw_vol                     ! Use remaining surface water for irrigation
            	surfaceWater(subws)%avail_sw_vol = 0.                                                                          ! Set remaining surface water to zero
    			else
    				  daily(ip)%tot_irr = 0.                                                  ! Irrigation set to zero when surface-water supplies are exceeded 
    			end if
    	case(2) ! Mixed Water Source
    			if (surfaceWater(subws)%avail_sw_vol >= (daily(ip)%tot_irr * fields(ip)%area)) then                                ! If available surface water exceeds demand
    			  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + (daily(ip)%tot_irr * fields(ip)%area)                  ! Add daily irrigation volume to total surface water irrigation 
    			  surfaceWater(subws)%avail_sw_vol = surfaceWater(subws)%avail_sw_vol - (daily(ip)%tot_irr * fields(ip)%area)      ! Update available surface water volume	
    			else if (surfaceWater(subws)%avail_sw_vol < (daily(ip)%tot_irr * fields(ip)%area) &                                ! If supply is less than demand but greater than 0
    				 .and. surfaceWater(subws)%avail_sw_vol /= 0) then
            	surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + surfaceWater(subws)%avail_sw_vol                     ! Use remaining surface water for irrigation
            	surfaceWater(subws)%avail_sw_vol = 0.                                                                          ! Set remaining surface water to zero
            	daily(ip)%gw_irr = daily(ip)%tot_irr - (surfaceWater(subws)%avail_sw_vol / fields(ip)%area)                    ! Use GW for remainder of demand
    			else
    				  daily(ip)%gw_irr =  daily(ip)%tot_irr                                   ! All irrigation assigned to groundwater well
    			end if 
    	  case(3) ! groundwater
    	  	daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well
    	  case(4) ! unknown, assume GW source
    	  	daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well
    	end select
    endif
    
  END SUBROUTINE IRRIGATION_RULESET
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE water_budget(ip,jday,month,moisture_save,MAR_active)
  
    INTEGER, INTENT (IN) :: ip, jday, month
    REAL, DIMENSION(npoly), INTENT(inout) :: moisture_save
    LOGICAL, INTENT(in)  :: MAR_active
    REAL:: rch
  
    daily(ip)%aET=min(daily(ip)%pET, previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr) 
    daily(ip)%deficiency=daily(ip)%pET-daily(ip)%aET
    if (daily(ip)%aET > 0) daily(ip)%ET_active =  1   ! Set ET flag to 1 if ET is active that day
    rch = max(0., (previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr-daily(ip)%aET)-&
    fields(ip)%whc*crops(fields(ip)%landcover_id)%RootDepth)
    if (rch > fields(ip)%max_infil_rate) then
    	daily(ip)%runoff = (rch - fields(ip)%max_infil_rate)
    	rch = fields(ip)%max_infil_rate
    endif
    daily(ip)%recharge = rch
    daily(ip)%swc=max(0.,previous(ip)%swc+daily(ip)%effprecip+daily(ip)%tot_irr-daily(ip)%aET-daily(ip)%recharge-daily(ip)%runoff)
    daily(ip)%residual = daily(ip)%swc-previous(ip)%swc+daily(ip)%aET+daily(ip)%recharge- &
                       daily(ip)%effprecip-daily(ip)%tot_irr-daily(ip)%runoff       
    if (MAR_active) moisture_save(ip) = previous(ip)%swc 
    previous(ip)%swc = daily(ip)%swc
    daily(ip)%change_in_storage = daily(ip)%effprecip+daily(ip)%tot_irr-daily(ip)%aET-daily(ip)%recharge-daily(ip)%runoff	
  
  END SUBROUTINE water_budget
  
END MODULE

