PROGRAM SWBM
  
! Scott valley soil water budget model (SWBM)
! 
! *****************************************************************************
! Input and Output is in Metric units:
! 
!                        [L]   =  meters
!                        [L^2] =  squared meters
!                        [L^3] =  cubic meters
!                        [T]   =  days
  
! subwn      : watershed name
!            : Scott=1, French=2, Etna=3, Patterson=4
!            : Kidder=5, Moffet=6, Mill=7, Shackleford =8, Scott Tailing=9
! landuse    : alfalfa/grain 11, pasture 2, ET/no_IRRIG 3, no_irrig/no_ET 4, water 6
! rotation   : alfalfa 11, grain 12
! irr_type   : flood 1, sprinkler 2, centerpivot 3, 555= n* in the DWR categories and should be non irrigated, 999= Unknown irrig
! irrigation efficiency coefficients: irreff_flood, irreff_sprink, irreff_cp
! area       : Area of each polygon
! watersource: SW=1, MIX=2, GW=3, SUB=4, dry=5, 999 = unknown
! whc        : water holding capacity
! Water source unknown (999)-> Groundwater 
! Irrigation type unknown (999)-> Wheel Line
! Water source dry or sub or n* (555) with any irrig type -> et/noIRR

  USE define_poly
  USE irrigationmodule
  USE outputmodule
  
  IMPLICIT NONE

  INTEGER  :: nmonth, numdays, imonth, jday, i, im, nrows, ncols, dummy
  INTEGER  :: n_wel_param, num_daily_out, unit_num, num_MAR_fields
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: zone_matrix, no_flow_matrix, output_zone_matrix, Discharge_Zone_Cells
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: MAR_fields, ip_daily_out
  REAL   :: precip, Total_Ref_ET, MAR_vol
  REAL, ALLOCATABLE, DIMENSION(:)  :: drain_flow, max_MAR_field_rate, moisture_save
  REAL :: start, finish
  INTEGER, ALLOCATABLE, DIMENSION(:)  :: ndays
  CHARACTER(10)  :: SFR_Template, scenario, suffix
  CHARACTER(50), ALLOCATABLE, DIMENSION(:) :: daily_out_name
  INTEGER, DIMENSION(31) :: ET_Active
  LOGICAL :: MAR_active, ILR_active, daily_out_flag
  REAL :: eff_precip
 
  CALL cpu_time(start)
  open (unit=800, file='SWBM_log.rec')                         ! Open record file for screen output
  eff_precip = 0.
  Total_Ref_ET = 0.
  
  open(unit=10, file='general_inputs.txt', status='old')
  read(10, *) npoly, nwells, nmonth, nrows, ncols, RD_Mult, SFR_Template, scenario
  close (10)
  if (trim(scenario)=='basecase' .or. trim(scenario)=='Basecase' .or. trim(scenario)=='BASECASE') then            ! Set logicals for Scenario type
    MAR_active=  .FALSE.  
    ILR_active = .FALSE.
  else if(trim(scenario)=='MAR' .or. trim(scenario)=='mar') then
    MAR_active=  .TRUE. 
    ILR_active = .FALSE.
  else if (trim(scenario)=='ILR' .or. trim(scenario)=='ilr') then
    MAR_active=  .FALSE.
  	ILR_active = .TRUE.
  else if (trim(scenario)=='MAR_ILR' .or. trim(scenario)=='mar_ilr') then
  	 MAR_active=  .TRUE.       
     ILR_active = .TRUE.    
  else if(trim(scenario).ne.'basecase' .or. trim(scenario).ne.'Basecase' .or. trim(scenario).ne.'BASECASE' &      ! Exit program if incorrect scenario type
         .or. trim(scenario).ne.'MAR' .or. trim(scenario).ne.'mar' &
         .or. trim(scenario).ne.'ILR' .or. trim(scenario).ne.'ilr' &
         .or. trim(scenario).ne.'MAR_ILR' .or. trim(scenario).ne.'mar_ilr' ) then
    write(*,*)'Unknown scenario input in general_inputs.txt'
    write(800,*)'Unknown scenario input in general_inputs.txt'
    CALL EXIT
  endif
    
  write(*,'(2a10)')'Scenario: ',trim(scenario)
  write(800,'(2a10)')'Scenario: ',trim(scenario)
  SFR_Template = TRIM(SFR_Template)
  write(*,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(800,'(A27, A6)') 'SFR Template File Format = ',SFR_Template
  write(*,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "nwells", nwells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult
  write(800,'(A5,I6,A15,I5,A8,I5,A23,F6.2)') "npoly", npoly, "nwells", nwells, "nmonth", nmonth,&
   "Root Depth Multiplier", RD_Mult 
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] open (unit=536, file="MNW2_template.txt", status="old")     
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] read(536,*) ! Read heading line into nothing
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] read(536,*)param_dummy,n_wel_param  ! read in number of well parameters (for printing later)
  ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] close(536)

  open(unit=11, file='ndays.txt', status = 'old')
  read(11,*)   ! read headers into nothing
  ALLOCATE(ndays(nmonth))
  do i=1, nmonth
  	read(11,*) dummy, ndays(i)
  enddo
  ALLOCATE(zone_matrix(nrows,ncols))
  ALLOCATE(no_flow_matrix(nrows,ncols))
  ALLOCATE(output_zone_matrix(nrows,ncols))
  ALLOCATE(Discharge_Zone_Cells(nrows,ncols))
  ALLOCATE(drain_flow(nmonth))
  
  open(unit=211,file='recharge_zones.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  read(211,*) zone_matrix
  close(211)
  open(unit=212,file='active_cell_matrix.txt',status='old')       ! Read in MODFLOW no flow cell matrix
  read(212,*) no_flow_matrix  
  close(212)
  output_zone_matrix = zone_matrix * no_flow_matrix        ! Create Recharge Zone Matrix with zeros at no flow cells

  CALL initialize_streams

  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] open(unit=218,file='ET_Cells_DZ.txt',status='old')      ! Read in MODFLOW recharge zone matrix
  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] read(218,*) Discharge_Zone_Cells
  ! [NEED TO RECODE FOR SIERRA VALLEY MODEL] close(218)
  
  CALL READ_KC_IRREFF                                       ! Read in crop coefficients and irrigation efficiencies
  CALL readpoly(npoly, nrows, ncols, output_zone_matrix)    ! Read in field info
  CALL read_well(npoly, nwells)                             ! Read in well info
  
  if (MAR_active) then
    open(unit=219,file='MAR_Fields.txt',status='old')      ! Read in MAR recharge matrix
    read(219,*) num_MAR_fields, MAR_vol
    ALLOCATE(MAR_fields(num_MAR_fields))                  ! Array of MAR field polygon IDs
    ALLOCATE(max_MAR_field_rate(num_MAR_fields))          ! Array of maximum infiltration rate for MAR fields (1/10th lowest SSURGO value)
    ALLOCATE(moisture_save(npoly))                        ! Array of soil-moisture needed to recalculate recharge for MAR fields
    moisture_save = 0.                                    ! Initialize array
    do i=1, num_MAR_fields
      read(219,*)MAR_fields(i), max_MAR_field_rate(i)
    enddo
    close(219)
  endif

  open(unit=887,file='precip.txt', status = 'old')                    
  open(unit=88,file='ref_et.txt', status = 'old')
  open(unit=79, file='kc_grain.txt', status = 'old')
  open(unit=80, file='kc_alfalfa.txt', status = 'old')
  open(unit=81, file='kc_pasture.txt', status = 'old')

  open(unit=599, file = 'print_daily.txt', status = 'old')
  read(599,*)num_daily_out, daily_out_flag
  ALLOCATE(ip_daily_out(num_daily_out))
  ALLOCATE(daily_out_name(num_daily_out))
  if (daily_out_flag) then
  	 do i=1, num_daily_out
  	 	 unit_num =  599 + i 
  	   read(599,*)ip_daily_out(i),daily_out_name(i)
  	   daily_out_name(i) = trim(daily_out_name(i)) // '_daily_out.dat'
  	   open(unit=unit_num, file=daily_out_name(i))
  	   write(unit_num,*)'field_id  precip_adj streamflow irrig  well rch moisture  ET',&
  	                    '  actualET  deficiency budget WHC subwn landuse rotation'    
    enddo
  endif
  
  CALL output_files(daily_out_flag)
  CALL EXECUTE_COMMAND_LINE('copy SVIHM_ETS_template.txt SVIHM.ets')
  CALL EXECUTE_COMMAND_LINE('copy SVIHM_SFR_template.txt SVIHM.sfr')
  CALL EXECUTE_COMMAND_LINE('copy SVIHM_WEL_template.txt SVIHM.wel')
  if (SFR_Template=='UCODE') then 
    CALL EXECUTE_COMMAND_LINE('copy SFR_UCODE_JTF.txt SVIHM_SFR.jtf')
  elseif (SFR_Template=='PEST') then
  	 CALL EXECUTE_COMMAND_LINE('copy SFR_PEST_TPL.txt SVIHM_SFR.tpl')
  else
    	write(*,*)'Invalid Template File Format Variable in general_inputs.txt'
    	write(800,*)'Invalid Template File Format Variable in general_inputs.txt'
    	CALL EXIT
  endif
  
  open (unit=220, file='Drains_m3day.txt')
  read(220,*)     ! Read header into nothing
  fields%irr_flag = 0          ! Initialize irrigation flag array
  do im=1, nmonth            ! Loop over each month
    imonth=MOD(im,12)        ! Create repeating integers for months (Oct=1, Nov=2, ..., Aug=11, Sep=0)
    numdays = ndays(im)      ! Number of days in the current month
    CALL zero_month                                 ! Zero out monthly accumulated volume
    if (imonth==1) CALL zero_year                             ! If October, Zero out yearly accumulated volume
    if (im==1) then 
      CALL do_rotation(im)                   ! populate initial fields%rotation values \
    else if (imonth==4 .and. im.ne.4) then
      CALL do_rotation(im)	                 ! Rotate alfalfa/grain in January, except for first year since rotation happened in October   
    endif                   
    CALL calc_area(im)                ! Calculate area for each month due to changing alfalfa/grain
    CALL read_monthly_stream_inflow(numdays)
    write(*,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', numdays
    write(800,'(a15, i3,a13,i2,a18,i2)')'Stress Period: ',im,'   Month ID: ',imonth,'   Length (days): ', numdays
    CALL zero_month                                 ! Zero out monthly accumulated volume
    if (imonth==1) then                             ! If October:
      CALL zero_year                                ! Zero out yearly accumulated volume
    endif    
    read(220,*)drain_flow(im)                       ! Read drain flow into array         
    do jday=1, numdays                              ! Loop over days in each month
      if (jday==1) monthly%ET_active = 0            ! Set ET counter to 0 at the beginning of the month. Used for turning ET on and off in MODFLOW so it is not double counted.    
      daily%ET_active  = 0                                 ! Reset ET active counter
      daily%irrigation = 0.                                ! Reset daily irrigation value to zero
      daily%well       = 0.                                ! Reset daily pumping value to zero
      daily%effprecip  = 0.                                ! Reset daily effective precip value to zero
      daily%evapotrasp = 0.                                ! Reset daily ET value to zero
      daily%recharge   = 0.                                ! Reset daily recharge value to zero 
      read(88,*) REF_ET
      Total_Ref_ET = Total_Ref_ET + REF_ET                 
      read(79,*) kc_grain
      read (80,*)kc_alfalfa
      read(81,*)kc_pasture
      read(887,*) precip
      if (precip .GE. (0.2*ref_et)) then
        eff_precip=precip
      else
        eff_precip=0.
      endif
	    do ip=1, npoly      
	       if (imonth==3 .and. jday==31 .and. ip==npoly) then ! If last day of the year, set irrigation flags and logical to zero
		       fields%irr_flag = 0         
		       irrigating = .false.           
	         print*, 'Irrigation Logical Reset'
	         write(800,*)'Irrigation Logical Reset'
	         CALL Update_Irr_Type(im)
	         print*, 'Irrigation Type Updated'    
           write(800,*)'Irrigation Type Updated'
	      endif
        if (ILR_active) then
          CALL IRRIGATION_ILR(ip, imonth, jday, eff_precip)
	      else
	        CALL IRRIGATION(ip, imonth, jday, eff_precip)
	      endif
	      CALL RECHARGE(ip,eff_precip,jday,imonth,moisture_save,MAR_active)
	      CALL deficiency_check(ip, imonth, jday)       
      enddo              ! End of polygon loop
      if (MAR_active) then 
        CALL MAR(imonth, num_MAR_fields, MAR_fields, max_MAR_field_rate, MAR_vol, eff_precip, jday, moisture_save)
      endif
      if (daily_out_flag) CALL daily_out(num_daily_out,ip_daily_out, eff_precip)              ! Print Daily Output for Selected Fields
      CALL pumping(jday, nwells, npoly, daily_out_flag)   ! Stream depletion subroutine
	    CALL monthly_SUM      ! add daily value to monthly total (e.g., monthly%irrigation = monthly%irrigation + daily%irrigation)
      CALL annual_SUM       ! add daily value to yearly total (e.g., yearly%irrigation = yearly%irrigation + daily%irrigation)
      if (jday==numdays) CALL SFR_streamflow(numdays)         ! Convert remaining surface water to SFR inflows at end of the month	
      if (MAR_active .and. jday==numdays) then
        write(*,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
        sum(monthly%MAR_vol)*0.000408734569/numdays,' cfs per day)'
        write(*,*)
        write(800,'(a13,f4.2,a6,f5.2,a13)')'MAR Volume = ',sum(monthly%MAR_vol)/1E6, ' Mm3 (', &
        sum(monthly%MAR_vol)*0.000408734569/numdays,' cfs per day)'  
        write(800,*)
      endif
    enddo             ! End of day loop
    CALL convert_length_to_volume
    CALL monthly_out_by_field(im)
    CALL monthly_pumping(numdays)
	  ! [NEED TO RECODE FOR SIERRA VALLEY] CALL ET_out_MODFLOW(im,imonth,numdays,nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells,npoly)
	  Total_Ref_ET = 0.  ! Reset monthly Average ET
	  CALL recharge_out_MODFLOW(im,numdays,nrows,ncols,output_zone_matrix)
    CALL monthly_volume_out
     ! [NEED TO RECODE FOR SIERRA VALLEY] CALL write_MODFLOW_SFR(im, nmonth, nSegs, SFR_Flows, drain_flow)
     ! [NEED TO RECODE FOR SIERRA VALLEY] CALL write_SFR_template (im, nmonth, nSegs, SFR_Flows, drain_flow, SFR_Template)   ! Write JTF file for UCODE 
     ! [NEED TO UPDATE CODE FOR MNW2 PACKAGE] CALL write_MODFLOW_WEL(im,imonth,nwells,n_wel_param)       
  enddo                  ! End of month loop
  close(84)
  close(60)
  close(61)		
  close(91)
  close(92)
  close(93)
  close(94)
  close(95)
  close(96)
  close(97)
  close(98)
  CALL cpu_time(finish)
  write(*,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
  write(800,'(A7,F6.2,A8)')'Time = ',((finish-start)/60),' minutes'
  
END PROGRAM SWBM