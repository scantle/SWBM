MODULE outputmodule

USE define_poly
USE irrigationmodule

IMPLICIT NONE

REAL :: scott_area, french_area,etna_area, patterson_area
REAL :: kidder_area, moffet_area, mill_area
REAL :: shackleford_area, scott_tailing_area
REAL :: alfalfa_irr_area, grain_irr_area, pasture_irr_area
REAL :: et_noirr_area, noet_noirr_area, water_area
REAL :: A_n_star_area, A_SUB_area, A_DRY_Area
REAL :: G_n_star_area, G_SUB_area, G_DRY_Area
REAL :: P_n_star_area, P_SUB_area, P_DRY_Area 
REAL :: LU3_area, Total_area
! REAL, DIMENSION(nInflowSegs) :: subwnwell, subwnactualET, subwnsw, subwnmoisture, subwnstorage
! REAL, DIMENSION(nInflowSegs) :: subwnirrig, subwnevapo, subwnrecharge, subwndeficiency
REAL, DIMENSION(nlanduse) :: landuseirrig, landuseevapo, landusedeficiency, landusesw
REAL, DIMENSION(nlanduse) :: landusewell, landuserecharge,landuseactualET, landusemoisture, landusestorage

CONTAINS

SUBROUTINE output_files(daily_out_flag)

  LOGICAL, INTENT(IN) :: daily_out_flag
  INTEGER :: i
  
  open(unit=900, file='Recharge_Total.dat', status = 'replace')   
  write(900,*)'Total_Recharge_m^3/day  Total_Recharge_m^3'            
  open(unit=84, file='SVIHM.rch', status = 'replace')                                                  
  open(unit=91, file='monthly_gw_normalized.dat', status = 'replace')              
  write(91,*)'Monthly groundwater applied to each field normalized to the field area (m)'
  write(91,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  open(unit=92, file='monthly_irrig_normalized.dat', status = 'replace')
  write(92,*)'Monthly irrigation applied to each field normalized to the field area (m)'
  write(92,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)    
  open(unit=93, file='monthly_pET_normalized.dat', status = 'replace')
  write(93,*)'Monthly potential ET for each field normalized to the field area (m)'
  write(93,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=94, file='monthly_recharge_normalized.dat', status = 'replace')
  write(94,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(94,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=95, file='monthly_storage_normalized.dat', status = 'replace')
  write(95,*)'Storage at the end of the month for each field normalized to the field area (m)'
  write(95,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=96, file='monthly_aET_normalized.dat', status = 'replace')
  write(96,*)'Monthly actual ET for each field normalized to the field area (m)'
  write(96,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=97, file='monthly_deficiency_normalized.dat', status = 'replace')
  write(97,*)'Monthly groundwater recharge from each field normalized to the field area (m)'
  write(97,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  
  open(unit=200, file='monthly_gw_volume.dat', status = 'replace')              
  write(200,*)'Monthly groundwater volume applied to each field (m^3)'
  write(200,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)
  open(unit=201, file='monthly_irrig_volume.dat', status = 'replace')
  write(201,*)'Monthly irrigation volume applied to each field (m^3)'
  write(201,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /)    
  open(unit=202, file='monthly_pET_volume.dat', status = 'replace')
  write(202,*)'Monthly potential ET for each field (m^3)'
  write(202,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=203, file='monthly_recharge_volume.dat', status = 'replace')
  write(203,*)'Monthly groundwater recharge from each field (m^3)'
  write(203,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=204, file='monthly_storage_volume.dat', status = 'replace')
  write(204,*)'Storage at the end of the month for each field (m^3)'
  write(204,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=205, file='monthly_aET_volume.dat', status = 'replace')
  write(205,*)'Monthly actual ET for each field (m^3)'
  write(205,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
  open(unit=206, file='monthly_deficiency_volume.dat', status = 'replace')
  write(206,*)'Monthly groundwater recharge from each field (m^3)'
  write(206,'(a14, 2119i5)')' Stress_Period',(/ (i, i = 1, npoly) /) 
                             
  open(unit=530, file='Monthly_Pumping_Volume_By_Well.dat', status = 'replace')
  write(530,*)'Monthly Pumping Volume (m^3) by well'
  write(530,'(172i6)')ag_wells(:)%well_id
  open(unit=531, file='Monthly_Pumping_Rate_By_Well.dat')
  write(531,*)'Monthly Pumping Rate (m^3/day) by well'
  write(531,'(172i6)')ag_wells(:)%well_id
  if (daily_out_flag) then
    open(unit=537, file='daily_pumping.dat', status = 'replace')
    write(537, *)"Daily pumping volume (m^3) for each well"
    write(537,'(999i12)')ag_wells(:)%well_id
  endif
  open(unit=120, file='ET_Active_Days.dat')                       
  write(120,'("Number of Days ET is Active in each polyon")')    

! ####################################### FIX #####################################################################  
!  open(unit=101, file='monthly_surfacewater_by_subw.dat')        
!  open(unit=102, file='monthly_groundwater_by_subw.dat')      
!  open(unit=103, file='monthly_irrigation_by_subw.dat')     
!  open(unit=104, file='monthly_pET_by_subw.dat')     
!  open(unit=105, file='monthly_aET_by_subw.dat')  
!  open(unit=106, file='monthly_recharge_by_subw.dat')  
!  open(unit=107, file='monthly_deficiency_by_subw.dat')
!  open(unit=108, file='monthly_storage_by_subw.dat')   

!  do i=101,108
!    write(i,*)'Stress_Period  Scott  French  Etna  Patterson  Kidder  Moffet  Mill  Shackleford  Scott_Tailings'
!  enddo
! #################################################################################################################
                                                       
  open(unit=109, file='monthly_surfacewater_by_luse.dat')        
  open(unit=110, file='monthly_groundwater_by_luse.dat')      
  open(unit=111, file='monthly_irrigation_by_luse.dat')     
  open(unit=112, file='monthly_pET_by_luse.dat')     
  open(unit=113, file='monthly_aET_by_luse.dat')  
  open(unit=114, file='monthly_recharge_by_luse.dat')  
  open(unit=115, file='monthly_deficiency_by_luse.dat')
  open(unit=116, file='monthly_storage_by_luse.dat')  
  
  do i=109,116
    write(i,*)'Stress_Period  Alfalfa  Grain  Pasture  ET_NoIrr  NoET_NoIrr'
  enddo
  
  open(unit=117, file='monthly_water_budget.dat')
  write(117,*)'Stress_Period Precip SW_Irr GW_Irr ET Recharge Storage'

  open(unit=532,file='5daysdeficiency.dat', status = 'replace')
  
  open(unit=60, file='subwatershed_area_m2.dat', status = 'replace')
  write(60,'(" Month Scott French Etna Patterson Kidder Moffet Mill Shackleford Tailings")')
  open(unit=61, file='landuse_area_m2.dat', status = 'replace')
  write(61,'(" Month Alfalfa Grain Pasture Et/noIrr noET/noIrr Water")')
  open(unit=62, file='landuse_area_m2_detailed.dat', status = 'replace')
  write(62,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')
  open(unit=63, file='landuse_area_acres_detailed.dat', status = 'replace')
  write(63,'(" SP A_Irr A_n* A_SUB A_DRY G_Irr G_n* G_SUB G_DRY P_Irr P_n* P_SUB P_DRY ET/noIrr noET/noIrr Water Total")')

END SUBROUTINE output_files
    
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE monthly_SUM

  ! Minor issue with change in storage and moisture. Change in storage is really the running total, need to fix it by differencing from month to month and also year to year.
  
  monthly%irrigation         = monthly%irrigation         + daily%irrigation        ! Add daily irrigation length to monthly total
  monthly%well               = monthly%well               + daily%well              ! Add daily pumping length to monthly total
  monthly%recharge           = monthly%recharge           + daily%recharge          ! Add daily recharge length to monthly total
  monthly%moisture           = monthly%moisture           + daily%change_in_storage !  
  monthly%evapotrasp         = monthly%evapotrasp         + daily%evapotrasp        ! Add daily ET length to monthly total
  monthly%actualET           = monthly%actualET           + daily%actualET          ! Add daily actualET length to monthly total
  monthly%deficiency         = monthly%deficiency         + daily%deficiency        ! Add daily deficiency length to monthly total
  monthly%ET_active          = monthly%ET_active          + daily%ET_active         ! Add daily ET length to monthly total    
  monthly%effprecip          = monthly%effprecip          + daily%effprecip         ! Add daily effective precip length to monthly total    
  monthly%change_in_storage  = monthly%change_in_storage  + daily%change_in_storage ! Add daily change in storage length to monthly total    
  monthly%MAR_vol            = monthly%MAR_vol            + daily%MAR_vol           ! Add daily MAR volume to monthly total

END SUBROUTINE monthly_SUM

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE annual_SUM

  yearly%irrigation          = yearly%irrigation          + daily%irrigation         ! Add daily irrigation length to yearly total
  yearly%well                = yearly%well                + daily%well               ! Add daily pumping length to yearly total   
  yearly%recharge            = yearly%recharge            + daily%recharge           ! Add daily recharge length to yearly total  
  yearly%moisture            = yearly%moisture            + daily%change_in_storage  ! Add daily moisture length to yearly total  
  yearly%evapotrasp          = yearly%evapotrasp          + daily%evapotrasp         ! Add daily ET length to yearly total        
  yearly%actualET            = yearly%actualET            + daily%actualET           ! Add daily actualET length to yearly total  
  yearly%deficiency          = yearly%deficiency          + daily%deficiency         ! Add daily deficiency length to yearly total
  yearly%ET_active           = yearly%ET_active           + daily%ET_active          ! Add daily ET length to yearly total    
  yearly%effprecip           = yearly%effprecip           + daily%effprecip          ! Add daily effective precip length to yearly total    
  yearly%change_in_storage   = yearly%change_in_storage   + daily%change_in_storage  ! Add daily change in storage length to yearly total    
  yearly%MAR_vol             = yearly%MAR_vol             + daily%MAR_vol            ! Add daily MAR to annual total

END SUBROUTINE annual_SUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE monthly_volume_out
  
  INTEGER :: i, ilanduse
  INTEGER, SAVE :: month = 0
  
  month = month+1
  ! subwnsw           = 0.
  ! subwnwell         = 0.                            
  ! subwnirrig        = 0.                         
  ! subwnevapo        = 0.                          
  ! subwnactualET     = 0.                       
  ! subwnrecharge     = 0.                    
  ! subwndeficiency   = 0.                 
  ! subwnmoisture     = 0.                  
   
  landusesw         = 0.                          
  landusewell       = 0.
  landuseirrig      = 0.
  landuseevapo      = 0.
  landuserecharge   = 0.
  landusedeficiency = 0.
  landuseactualET   = 0.
  landusemoisture   = 0.      

  do i = 1, npoly
  ! subwnsw(fields(i)%subwn)         = subwnsw(fields(i)%subwn) &
  !                                   + (monthly(i)%irrigation_vol - monthly(i)%well_vol)
  ! subwnwell(fields(i)%subwn)       = subwnwell(fields(i)%subwn)       + monthly(i)%well_vol  
  ! subwnirrig(fields(i)%subwn)      = subwnirrig(fields(i)%subwn)      + monthly(i)%irrigation_vol
  ! subwnevapo(fields(i)%subwn)      = subwnevapo(fields(i)%subwn)      + monthly(i)%evapotrasp_vol
  ! subwnactualET(fields(i)%subwn)   = subwnactualET(fields(i)%subwn)   + monthly(i)%actualET_vol
  ! subwnrecharge(fields(i)%subwn)   = subwnrecharge(fields(i)%subwn)   + monthly(i)%recharge_vol
  ! subwndeficiency(fields(i)%subwn) = subwndeficiency(fields(i)%subwn) + monthly(i)%deficiency_vol   
  ! subwnmoisture(fields(i)%subwn)   = subwnmoisture(fields(i)%subwn)   + monthly(i)%moisture_vol

  select case (fields(i)%landuse)
    case (11)   !alfalfa / grain
      if (fields(i)%irr_type == 555 .or. fields(i)%water_source == 4 .or. fields(i)%water_source == 5) then  ! If n* or SUB or DRY
        ilanduse = 4  ! ET_noIRRIG
      else
        if (fields(i)%rotation==11) ilanduse = 1  ! Alfalfa
        if (fields(i)%rotation==12) ilanduse = 2  ! Grain 
      endif
    case (2)
      if (fields(i)%irr_type == 555 .or. fields(i)%water_source == 4 .or. fields(i)%water_source == 5) then  ! If n* or SUB or DRY
        ilanduse = 4  ! ET_noIRRIG
      else
        ilanduse = 3 ! pasture
      endif
    case(3)
       ilanduse = 4  ! ET_noIRRIG
       if (daily(i)%irrigation .GT. 0) write(800,*)'Polygon',i
       if (monthly(i)%irrigation_vol .GT. 0) write(800,*)'Polygon',i
    case (4)
       ilanduse = 5  ! noET_noIRRIG     
    end select 
  
  landusesw(ilanduse)         = landusesw(ilanduse)  &
                                + (monthly(i)%irrigation_vol - monthly(i)%well_vol)
  landusewell(ilanduse)       = landusewell(ilanduse)       + monthly(i)%well_vol 
  landuseirrig(ilanduse)      = landuseirrig(ilanduse)      + monthly(i)%irrigation_vol
  landuseevapo(ilanduse)      = landuseevapo(ilanduse)      + monthly(i)%evapotrasp_vol
  landuserecharge(ilanduse)   = landuserecharge(ilanduse)   + monthly(i)%recharge_vol
  landusedeficiency(ilanduse) = landusedeficiency(ilanduse) + monthly(i)%deficiency_vol
  landuseactualET(ilanduse)   = landuseactualET(ilanduse)   + monthly(i)%actualET_vol   
  landusemoisture(ilanduse)   = landusemoisture(ilanduse)   + monthly(i)%moisture_vol
  enddo    
                                      
  ! write(101,'(i4,9F20.8)') month, subwnsw(:)                         
  ! write(102,'(i4,9F20.8)') month, subwnwell(:)                     
  ! write(103,'(i4,9F20.8)') month, subwnirrig(:)                    
  ! write(104,'(i4,9F20.8)') month, subwnevapo(:)                    
  ! write(105,'(i4,9F20.8)') month, subwnactualET(:)                 
  ! write(106,'(i4,9F20.8)') month, subwnrecharge(:)                 
  ! write(107,'(i4,9F20.8)') month, subwndeficiency(:)               
  ! write(108,'(i4,9F20.8)') month, subwnmoisture(:)                 
                                                                    
  write(109,'(i4,5F20.8)') month, landusesw(:)                     
  write(110,'(i4,5F20.8)') month, landusewell(:)                   
  write(111,'(i4,5F20.8)') month, landuseirrig(:)                                                                    
  write(112,'(i4,5F20.8)') month, landuseevapo(:)                      
  write(113,'(i4,5F20.8)') month, landuseactualET(:)                   
  write(114,'(i4,5F20.8)') month, landuserecharge(:)                  
  write(115,'(i4,5F20.8)') month, landusedeficiency(:)               
  write(116,'(i4,5F20.8)') month, landusemoisture(:)                                          
                                                                      
  write(117,'(i4,6F20.0)')month, sum(monthly%effprecip_vol), (sum(monthly%irrigation_vol)-sum(monthly%well_vol)), &
  sum(monthly%well_vol), -sum(monthly%actualET_vol), -sum(monthly%recharge_vol), -sum(monthly%change_in_storage_vol)
  
END SUBROUTINE monthly_volume_out
    
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	

SUBROUTINE pumping(jday, nwells, npoly, daily_out_flag)

  INTEGER, INTENT(IN) :: jday, nwells, npoly
  LOGICAL, INTENT(IN) :: daily_out_flag
  INTEGER :: i, j, well_idx
  
  ag_wells%daily_well_vol = 0.  ! set daily value to zero
  if (jday ==1) then                       ! If first day of month, reset monthly values to zero
    ag_wells%monthly_well_vol = 0.
    ag_wells%monthly_well_rate = 0.
  end if
  
  do i=1, npoly
  	well_idx = fields(i)%well_idx
  	ag_wells(well_idx)%daily_well_vol =  ag_wells(well_idx)%daily_well_vol + &
       daily(ip)%well*fields(ip)%MF_area   ! assign daily pumping volume
    do j=1,nwells     
      ag_wells(well_idx)%monthly_well_vol = ag_wells(well_idx)%monthly_well_vol + &
      ag_wells(well_idx)%daily_well_vol ! add daily volume to monthly counter
    enddo
  enddo
  
  if(daily_out_flag) write(537,'(200es20.8)') ag_wells%daily_well_vol
    
END SUBROUTINE pumping 

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
SUBROUTINE monthly_pumping(numdays)

  INTEGER, INTENT (IN) :: numdays
  
  ag_wells(:)%monthly_well_rate = ag_wells(:)%monthly_well_vol / numdays
  
  write(530,'(172es20.8)')ag_wells(:)%monthly_well_vol
  write(531,'(172es20.8)')ag_wells(:)%monthly_well_rate
    

END SUBROUTINE monthly_pumping
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE write_MODFLOW_WEL(im,month,nwells,n_wel_param)
  
  INTEGER, INTENT(IN) :: im, month, nwells, n_wel_param
  INTEGER :: i, well_idx
  
  open(unit=536, file='SVIHM.wel',Access = 'append', status='old')
  if (month == 1 .or. month == 2 .or. month == 3 .or. month == 4 .or. &                              ! If October-March
      month == 5 .or. month == 6) then
    write(536,'(I10,I10,A28,I4)')nwells, n_wel_param-2, '               Stress Period',im         ! Only MFR is active, subtract number of ditches represented 
  else if (month == 7 .or. month == 8) then                                                            ! If April-May
    write(536,'(I10,I10,A28,I4)')nwells, n_wel_param, '               Stress Period',im           ! MFR and Ditches are active, use all WEL parameters
  else if (month == 9 .or. month == 10) then                                                           ! If June - July
    write(536,'(I10,I10,A28,I4)')nwells, n_wel_param-7, '               Stress Period',im         ! Only Ditches are active, subtract number of MFR segments represented
  else if (month == 11 .or. month == 0) then                                                           ! If August-September
    write(536,'(I10,I10,A28,I4)')nwells, n_wel_param-9, '               Stress Period',im         ! Only Ditches are active, subtract number of MFR and Ditch segments represented                                              
  end if
  
  do i=1,nwells
  	well_idx = fields(i)%well_idx
    write(536,'(3I10,ES15.3)')ag_wells(well_idx)%layer, ag_wells(well_idx)%well_row, &
     ag_wells(well_idx)%well_col, -1*ag_wells(well_idx)%monthly_well_rate
  enddo
  
  if (month == 1 .or. month == 2 .or. month == 3 .or. month == 4 .or. &                              ! If October-March MFR is active
      month == 5 .or. month == 6) then                                                           
    write(536,*)'  MFR5'
    write(536,*)'  MFR6'
    write(536,*)'  MFR7'
    write(536,*)'  MFR8'
    write(536,*)'  MFR9'
    write(536,*)'  MFR10'
    write(536,*)'  MFR11'
  else if (month == 7 .or. month == 8) then                                                            ! If April-May MFR and Ditches are active
    write(536,*)'  MFR5'                                            
    write(536,*)'  MFR6'
    write(536,*)'  MFR7'
    write(536,*)'  MFR8'
    write(536,*)'  MFR9'
    write(536,*)'  MFR10'
    write(536,*)'  MFR11'
    write(536,*)'  FRMRSDitch'
    write(536,*)'  SVIDDitch'
  else if (month == 9 .or. month == 10) then                                                           ! If June-July ditches are active
    write(536,*)'  FRMRSDitch'
    write(536,*)'  SVIDDitch'
  end if
  
  END SUBROUTINE  write_MODFLOW_WEL
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   SUBROUTINE daily_out(num_daily_out,ip_daily_out, eff_precip)
   
   INTEGER, INTENT(in) ::  num_daily_out
   INTEGER, DIMENSION(num_daily_out), INTENT(in) :: ip_daily_out
   INTEGER  :: unit_num, i
   REAL, INTENT(in) :: eff_precip
   
   do i=1,num_daily_out
     unit_num = 599+i
     write(unit_num,'(i5,11F20.8,3i4)') ip_daily_out(i), eff_precip, streamflow_in(fields(ip_daily_out(i))%SFR_seg),      &
                                        daily(ip_daily_out(i))%irrigation, daily(ip_daily_out(i))%well,          &
                                        daily(ip_daily_out(i))%recharge, daily(ip_daily_out(i))%moisture,        & 
                                        daily(ip_daily_out(i))%evapotrasp,  daily(ip_daily_out(i))%actualET,     &  
                                        daily(ip_daily_out(i))%deficiency,daily(ip_daily_out(i))%budget,         &
                                        fields(ip_daily_out(i))%whc,fields(ip_daily_out(i))%SFR_seg,                   &
                                        fields(ip_daily_out(i))%landuse, fields(ip_daily_out(i))%rotation
   enddo                                                                                                   ! Field IDs for Daily Output
   END SUBROUTINE daily_out
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! The following version of the daily out SUBROUTINE saves daily values for each polygon
! but makes the program very slow and should be done only when there is the specific 
! purpose of saving all these values

!    SUBROUTINE daily_out( ip, eff_precip)!

!      INTEGER::ip
!      REAL :: eff_precip
!      character(len=9) :: fileroot
!      character(len=13) :: outday
         
!      fileroot = "day_poly."

!      write(outday,'(a9,i4)') fileroot, ip
!     print *,outday

!      open (unit = 600, file = outday, position="append")

!      write(600,'(i5,10f16.5,2i4)') ip, eff_precip,streamflow_in(fields(ip)%SFR_seg), &
!                               daily(ip)%irrigation, daily(ip)%well,       &
!                               daily(ip)%recharge, daily(ip)%moisture,     &
!                               daily(ip)%evapotrasp, daily(ip)%actualET,   &
!                               daily(ip)%deficiency,  fields(ip)%whc,        &
!                               fields(ip)%SFR_seg, fields(ip)%landuse
!      close(600)
!    END SUBROUTINE daily_out
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE monthly_out_by_field(im)                                  
                                                            
    INTEGER, INTENT(in) :: im                               
                                                                         
    write(91,'(i4,9999g12.5)')im, monthly%well               
    write(92,'(i4,9999g12.5)')im, monthly%irrigation      
    write(93,'(i4,9999g12.5)')im, monthly%evapotrasp                   
    write(94,'(i4,9999g12.5)')im, monthly%recharge        
    write(95,'(i4,9999g12.5)')im, daily%moisture             ! daily value used because that is the state of the field at the end of the month 
    write(96,'(i4,9999g12.5)')im, monthly%actualET        
    write(97,'(i4,9999g12.5)')im, monthly%deficiency                           
    write(120,'(i4,2119i4)')im, monthly%ET_active        
                                                                         
    write(200,'(i4,9999g12.5)')im, monthly%well_vol          
    write(201,'(i4,9999g12.5)')im, monthly%irrigation_vol 
    write(202,'(i4,9999g12.5)')im, monthly%evapotrasp_vol                  
    write(203,'(i4,9999g12.5)')im, monthly%recharge_vol   
    write(204,'(i4,9999g12.5)')im, daily%moisture*fields%MF_area       ! daily value used because that is the state of the field at the end of the month 
    write(205,'(i4,9999g12.5)')im, monthly%actualET_vol                    
    write(206,'(i4,9999g12.5)')im, monthly%deficiency_vol
        
    END SUBROUTINE monthly_out_by_field

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    SUBROUTINE ET_out_MODFLOW(im,month,nday,nrows,ncols,output_zone_matrix,Total_Ref_ET,Discharge_Zone_Cells, npoly)
 
    INTEGER, INTENT(IN) :: im,month,nrows,ncols, npoly
    INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols), nday(0:11), Discharge_Zone_Cells(nrows,ncols)
    REAL, INTENT(IN) :: Total_Ref_ET
    REAL :: Avg_Ref_ET
    REAL, DIMENSION(npoly) :: ET_fraction
    INTEGER :: ip
    REAL, DIMENSION(nrows,ncols) :: Extinction_depth_matrix, ET_matrix_out
    
    ET_matrix_out = 0.
    Avg_Ref_ET = Total_Ref_ET/dble(nday(month))                                                   ! Calculate average Reference ET for populating ET package
    
    do ip=1,npoly
      ET_fraction(ip) = monthly(ip)%ET_active / dble(nday(month)) 
      where (output_zone_matrix(:,:) == ip)
        ET_matrix_out(:,:) = Avg_Ref_ET * (1 - ET_fraction(ip))  ! Scale Average monthly ET by the number of days ET was not active on the field.
        Extinction_depth_matrix(:,:) = 0.5
      end where
    enddo
    
    ET_matrix_out = ET_matrix_out * Discharge_Zone_Cells
    Extinction_depth_matrix = Extinction_depth_matrix 
    
    if (im==1) then
    	open(unit=83, file='SVIHM.ets', status = 'old', position = 'append')
    	write(83, *)"       20   1.00000(10e14.6)                   -1     ET RATE"
      write(83,'(10e14.6)') ET_matrix_out                ! Write Max ET Rates 
      write(83,*)'       20   1.00000(10e14.6)                   -1     ET DEPTH'
      write(83,'(10e14.6)') Extinction_depth_matrix  ! Write ET Extinction Depth
      write(83,*)'        05.0000e-01(20F6.3)                    -1     PXDP Segment 1'    ! Linear decrease in ET from land surface to 0.5 m below surface
      write(83,*)'        05.0000e-01(20F6.3)                    -1     PETM Segment 1'    ! Linear decrease in ET from land surface to 0.5 m below surface
    else
      write(83,*)'       -1         1         1        -1        -1'	 
      write(83, *)"       20   1.00000(10e14.6)                   -1     ET RATE"
      write(83,'(10e14.6)') ET_matrix_out                ! Write Max ET Rates 
      write(83,*)'       20   1.00000(10e14.6)                   -1     ET DEPTH'
      write(83,'(10e14.6)') Extinction_depth_matrix  ! Write ET Extinction Depth
    end if
    END SUBROUTINE ET_out_MODFLOW	

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    SUBROUTINE recharge_out_MODFLOW(im,numdays,nrows,ncols,output_zone_matrix)  ! Recharge file            

      
      INTEGER, INTENT(IN) :: im,nrows,ncols, numdays
      INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols)
      REAL, ALLOCATABLE, DIMENSION(:,:) ::  recharge_matrix
      INTEGER :: ip
      REAL :: ttl_rch
  
      ALLOCATE(recharge_matrix(nrows,ncols))
      recharge_matrix = 0.
      
      if (im == 1) then
      write(84,'(a31)')'# Recharge File written by SWBM'
      write(84,*)'PARAMETER  0'
      write(84,*)'3  50'
      end if
      
      write(84,*)'1  0'
      write(84,'(a63)')'        18   1.00000(10e14.6)                   -1     RECHARGE'
       
        do ip = 1, npoly
          where (output_zone_matrix(:,:) == ip) 
            recharge_matrix(:,:) = monthly(ip)%recharge / numdays
          end where
        enddo
      write(84,'(10e14.6)') recharge_matrix   
      ttl_rch = sum(recharge_matrix*10000)
      write(900,*) ttl_rch, ttl_rch*numdays
    END SUBROUTINE recharge_out_MODFLOW
    
! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   
!     SUBROUTINE recharge_out_MODFLOW_w_MAR(im,month,nday,nrows,ncols,output_zone_matrix,MAR_Matrix)  ! Recharge file  with MAR added during Jan Feb and Mar          
! 
!       
!       INTEGER, INTENT(IN) :: im,month,nrows,ncols
!       INTEGER, INTENT(IN) :: output_zone_matrix(nrows,ncols), nday(0:11)
!       REAL, INTENT(IN) :: MAR_Matrix(nrows,ncols)
!       REAL, ALLOCATABLE, DIMENSION(:,:) ::  recharge_matrix
!       INTEGER :: ip
!       REAL :: rch_sum, MAR_sum, ttl_rch
!   
!       ALLOCATE(recharge_matrix(nrows,ncols))
!       recharge_matrix = 0.
!       
!       if (im == 1) then
!       write(84,'(a31)')'# Recharge File written by SWBM'
!       write(84,*)'PARAMETER  0'
!       write(84,*)'3  50'
!       end if
!       
!       write(84,*)'1  0'
!       write(84,'(a63)')'        18   1.00000(10e14.6)                   -1     RECHARGE'
!        
!         do ip = 1, npoly
!           where (output_zone_matrix(:,:) == ip) 
!             recharge_matrix(:,:) = monthly(ip)%recharge / nday(month)
!           end where
!         enddo
!         if (month == 4 .or. month == 5 .or. month == 6) then          ! If Jan-Mar add recharge from MAR
!           rch_sum = sum(recharge_matrix*10000)                           ! Total of normal recharge rate in m^3/day
!           MAR_sum = sum(MAR_Matrix*10000)                                ! Total of MAR rate in m^3/day
!           ttl_rch = rch_sum + MAR_sum                                    ! Total recharge rate applied to MODFLOW
!           write(*,'(a15,f12.0,a8)')'Non-MAR rate = ',rch_sum,' m^3/day'
!           write(800,'(a15,f12.0,a8)')'Non-MAR rate = ',rch_sum,' m^3/day'
!           recharge_matrix(:,:) = recharge_matrix(:,:) + MAR_Matrix
!           write(*,'(a11,f12.0,a8)')'MAR rate = ',MAR_sum,' m^3/day'
!           write(800,'(a11,f12.0,a8)')'MAR rate = ',MAR_sum,' m^3/day'
!           write(*,'(a22,f12.0,a8)')'Total recharge rate = ',ttl_rch, ' m^3/day'
!           write(800,'(a22,f12.0,a8)')'Total recharge rate = ',ttl_rch, ' m^3/day'
!           write(*,*)''
!           write(800,*),''
!         else
!           ttl_rch = sum(recharge_matrix*10000)
!         end if
!       write(84,'(10e14.6)') recharge_matrix      
!       write(900,*) ttl_rch
!     END SUBROUTINE recharge_out_MODFLOW_w_MAR
!
!!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
    SUBROUTINE convert_length_to_volume

    monthly%irrigation_vol         = monthly%irrigation        *fields%MF_area
    monthly%evapotrasp_vol         = monthly%evapotrasp        *fields%MF_area
    monthly%moisture_vol           = monthly%moisture          *fields%MF_area
    monthly%actualET_vol           = monthly%actualET          *fields%MF_area
    monthly%recharge_vol           = monthly%recharge          *fields%MF_area
    monthly%well_vol               = monthly%well              *fields%MF_area
    monthly%deficiency_vol         = monthly%deficiency        *fields%MF_area      
    monthly%effprecip_vol          = monthly%effprecip         *fields%MF_area      
    monthly%change_in_storage_vol  = monthly%change_in_storage *fields%MF_area

    yearly%irrigation_vol          = yearly%irrigation         *fields%MF_area
    yearly%evapotrasp_vol          = yearly%evapotrasp         *fields%MF_area
    yearly%moisture_vol            = yearly%moisture           *fields%MF_area
    yearly%actualET_vol            = yearly%actualET           *fields%MF_area
    yearly%recharge_vol            = yearly%recharge           *fields%MF_area
    yearly%well_vol                = yearly%well               *fields%MF_area
    yearly%deficiency_vol          = yearly%deficiency         *fields%MF_area
    yearly%effprecip_vol           = yearly%effprecip          *fields%MF_area      
    yearly%change_in_storage_vol   = yearly%change_in_storage  *fields%MF_area
    
   END SUBROUTINE convert_length_to_volume

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   SUBROUTINE calc_area(im)

    integer ::  ip, im
 
    scott_area         = 0.
    french_area        = 0.
    etna_area          = 0.
    patterson_area     = 0.
    kidder_area        = 0.
    moffet_area        = 0.
    mill_area          = 0.
    shackleford_area   = 0.
    scott_tailing_area = 0.

    alfalfa_irr_area   = 0.
    grain_irr_area     = 0.
    pasture_irr_area   = 0.
    et_noirr_area      = 0.
    noet_noirr_area    = 0.
	  water_area         = 0.
	  A_n_star_area      = 0.
	  A_SUB_area         = 0.
	  A_DRY_area         = 0.
	  G_n_star_area      = 0.
	  G_SUB_area         = 0.
	  G_DRY_area         = 0.
	  P_n_star_area      = 0.
	  P_SUB_area         = 0.
	  P_DRY_area         = 0.
    LU3_area           = 0.
    do ip = 1, npoly
    
       select case (fields(ip)%landuse)
       case (11)   ! alfalfa / grain
           if (fields(ip)%rotation==11) then
             if (fields(ip)%irr_type == 555) then               ! If n* (non-irrigated alfalfa)
               et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               A_n_star_area = A_n_star_area + fields(ip)%MF_area  ! Add area to alfalfa n* sub category
             else if (fields(ip)%water_source == 4) then        ! If SUB (non-irrigated alfalfa)
           	   et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               A_SUB_area = A_SUB_area + fields(ip)%MF_area	      ! Add area to  alfalfa sub-irrigated sub category
             else if (fields(ip)%water_source == 5) then        ! If DRY (non-irrigated alfalfa)
               et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               A_DRY_area = A_DRY_area + fields(ip)%MF_area        ! Add area to alfalfa dry sub category
             else
             	 alfalfa_irr_area = alfalfa_irr_area + fields(ip)%MF_area    ! Add area to irrigated alfalfa category
             end if
           else if (fields(ip)%rotation==12) then
             if (fields(ip)%irr_type == 555) then               ! If n* (non-irrigated grain)
               et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               G_n_star_area = G_n_star_area + fields(ip)%MF_area  ! Add area to  grain n* sub category
             else if (fields(ip)%water_source == 4) then        ! If SUB (non-irrigated grain)
           	   et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               G_SUB_area = G_SUB_area + fields(ip)%MF_area	      ! Add area to grain sub-irrigated sub category
             else if (fields(ip)%water_source == 5) then        ! If DRY (non-irrigated grain)
               et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
               G_DRY_area = G_DRY_area + fields(ip)%MF_area        ! Add area to grain dry sub category
             else
             	 grain_irr_area = grain_irr_area + fields(ip)%MF_area	      ! Add area to irrigated grain category
             end if	  
           else
           	write(*,*)'SOMETHING IS ROTTEN IN DENMARK. ALFALFA-GRAIN ROTATION ERROR.'
           	write(800,*)'SOMETHING IS ROTTEN IN DENMARK. ALFALFA-GRAIN ROTATION ERROR.'
           	CALL EXIT
           end if
       case (2)    ! pasture
           if (fields(ip)%irr_type == 555) then               ! If n* (non-irrigated alfalfa/grain)
             et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
             P_n_star_area = P_n_star_area + fields(ip)%MF_area  ! Add area to n* sub category
           else if (fields(ip)%water_source == 4) then        ! If SUB (non-irrigated alfalfa/grain)
           	 et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
             P_SUB_area = P_SUB_area + fields(ip)%MF_area	      ! Add area to sub-irrigated sub category
           else if (fields(ip)%water_source == 5) then        ! If DRY (non-irrigated alfalfa/grain)
             et_noirr_area = et_noirr_area + fields(ip)%MF_area	! Add area to ET/noIrr
             P_DRY_area = P_DRY_area + fields(ip)%MF_area        ! Add area to dry sub category
           else                                             
             pasture_irr_area=pasture_irr_area + fields(ip)%MF_area      ! Add area to irrigated pasture category   
           endif
       case (3)    !ET_noIRR
         et_noirr_area  = et_noirr_area + fields(ip)%MF_area     ! Total area of ET/noIrr including n*, SUB, and DRY fields
         LU3_area  = LU3_area + fields(ip)%MF_area               ! Original area of Landuse = 3 in polygon input table    
       case (4)    !noET_noIRR
         noet_noirr_area  = noet_noirr_area + fields(ip)%MF_area
	   case (6)    !water
         water_area  = water_area + fields(ip)%MF_area
       end select

       select case ( fields(ip)%SFR_seg)
        case (1)
         scott_area = scott_area + fields(ip)%MF_area 
        case (2)
         french_area =  french_area + fields(ip)%MF_area
         case (3)
       etna_area  =  etna_area + fields(ip)%MF_area
         case (4)
       patterson_area =  patterson_area + fields(ip)%MF_area
         case (5)
       kidder_area =  kidder_area + fields(ip)%MF_area
         case (6) 
       moffet_area =  moffet_area + fields(ip)%MF_area
         case (7)
       mill_area   =  mill_area + fields(ip)%MF_area
         case (8)
       shackleford_area = shackleford_area + fields(ip)%MF_area
         case (9)
       scott_tailing_area =  scott_tailing_area + fields(ip)%MF_area

       end select
	   
    enddo

    Total_area = alfalfa_irr_area + A_n_star_area + A_SUB_area + A_DRY_area + &
                                grain_irr_area + G_n_star_area + G_SUB_area + G_DRY_area + &
                                pasture_irr_area + P_n_star_area + P_SUB_area + P_DRY_area + &
                                LU3_area + noet_noirr_area + water_area
    write(60,'(i3,9F20.8)') im ,scott_area, french_area,  etna_area, patterson_area, kidder_area,  moffet_area,  mill_area, &
                            shackleford_area,  scott_tailing_area
    write(61,'(i3,6F20.8)') im, alfalfa_irr_area,  grain_irr_area,  pasture_irr_area,  et_noirr_area,  noet_noirr_area, water_area
    write(62,'(i4,16F20.4)') im, alfalfa_irr_area, A_n_star_area, A_SUB_area, A_DRY_area, &
                                grain_irr_area, G_n_star_area, G_SUB_area, G_DRY_area, &
                                pasture_irr_area, P_n_star_area, P_SUB_area, P_DRY_area, &
                                LU3_area, noet_noirr_area, water_area, Total_area
    
    alfalfa_irr_area  = alfalfa_irr_area * 0.000247105 ! Convert area from m^2 to acres
    A_n_star_area     = A_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    A_SUB_area        = A_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    A_DRY_area        = A_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    grain_irr_area    = grain_irr_area   * 0.000247105 ! Convert area from m^2 to acres
    G_n_star_area     = G_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    G_SUB_area        = G_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    G_DRY_area        = G_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    pasture_irr_area  = pasture_irr_area * 0.000247105 ! Convert area from m^2 to acres
    P_n_star_area     = P_n_star_area    * 0.000247105 ! Convert area from m^2 to acres
    P_SUB_area        = P_SUB_area       * 0.000247105 ! Convert area from m^2 to acres
    P_DRY_area        = P_DRY_area       * 0.000247105 ! Convert area from m^2 to acres
    LU3_area          = LU3_area         * 0.000247105 ! Convert area from m^2 to acres
    noet_noirr_area   = noet_noirr_area  * 0.000247105 ! Convert area from m^2 to acres
    water_area        = water_area       * 0.000247105 ! Convert area from m^2 to acres
    Total_area        = Total_area       * 0.000247105 ! Convert area from m^2 to acres
    
    write(63,'(i4,16F20.4)') im, alfalfa_irr_area, A_n_star_area, A_SUB_area, A_DRY_area, &
                                grain_irr_area, G_n_star_area, G_SUB_area, G_DRY_area, &
                                pasture_irr_area, P_n_star_area, P_SUB_area, P_DRY_area, &
                                LU3_area, noet_noirr_area, water_area, Total_area
   END SUBROUTINE calc_area
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  SUBROUTINE write_MODFLOW_SFR (im, nmonths, nSegs, SFR_Flows, drain_flow)
! In this SUBROUTINE the input file for the SFR package is created.  
  INTEGER :: im, i, j, nmonths, nSegs,  SR_width, Trib_width
  INTEGER,dimension(32,2) :: seg
  REAL, DIMENSION(1:32) :: SFR_Flows
  REAL, DIMENSION(nmonths) :: drain_flow
  
  
  seg(:,1) = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32/)    ! SFR Segment Number [nseg]
  seg(:,2) = (/3,3,4,5,9,8,8,9,10,12,12,14,14,23,17,17,19,19,22,26,22,23,25,25,26,30,29,29,30,0,0,0/)      ! SFR Inflow Segment [outseg]
  SR_width = 25
  Trib_width = 10

  if (im==1) then
    open(unit=213, file='SVIHM.sfr',Access = 'append', status='old')
    write(213,*)
    write(213,'(I4,A12)')nSegs,'  1  0  0  0'      ! Positive value after nSegs suppresses printing of SFR input data to listing file
  else
  	open(unit=213, file='SVIHM.sfr',Access = 'append', status='old')
    write(213,'(I4,A12)')nSegs,'  1  0  0  0'      ! Positive value after nSegs suppresses printing of SFR input data to listing file
  end if
  
  do j = 1, nSegs
    if (j==1) then  ! EF + SF Inflow  	    
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0',SFR_Flows(1),'  0  0  0  0.035'
      write(213,'(I4)')SR_width
      write(213,'(I4)')SR_width
    else if (j==2) then ! Sugar Creek Inflow 
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(2),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==3) then ! Last Tailings Segment 
      write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
      write(213,'(I4)')SR_width
      write(213,'(I4)')SR_width
    else if (j==4 .or. j==5 .or. j==9 .or. j==10 .or. j==12 .or. j==14 .or. j==23 .or. j==25 .or. j==26 .or. j==30) then ! Scott River Segments
  	  if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        else
      	  write(213,'(I3,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(213,'(I4,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        else
      	  write(213,'(I4,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')SR_width
          write(213,'(I4)')SR_width
        end if
      end if
    else if (j==6) then ! French Creek Branch #1
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(6),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==7) then ! French Creek Branch #2	
      write(213,'(I3,A3,I3,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(7),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==8 .or. j==13 .or. j==17 .or. j==20 .or. j==22 .or. j==29) then ! Tributaries and Sloughs, Oro Fino Creek (no inflow)
      if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(213,'(I3,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        else
      	  write(213,'(I3,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(213,'(I4,A3,I3,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        else
      	  write(213,'(I4,A3,I4,A29)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  0.035'
          write(213,'(I4)')Trib_width
          write(213,'(I4)')Trib_width
        end if	
      end if
    else if (j==11) then ! Etna Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(11),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width	
    else if (j==15) then ! Johnson Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(15),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==16) then ! Crystal Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(16),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==18) then ! Patterson Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(18),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==19) then ! Big Slough Drain inflow segment
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', drain_flow(im),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==21) then ! Kidder Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(21),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==24) then ! Moffett Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(24),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==27) then ! Mill Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(27),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==28) then ! Shackleford Creek
      write(213,'(I4,A3,I4,A3,es10.2,A16)')j,'  1', seg(j,2),'  0', SFR_Flows(28),'  0  0  0  0.035'
      write(213,'(I4)')Trib_width
      write(213,'(I4)')Trib_width
    else if (j==31) then ! Farmers Ditch Diversion 
      write(213,'(I4,A3,I3,A6,es10.2,A16)')j,'  1', seg(j,2),'  3  0', SFR_Flows(31),'  0  0  0  0.035'
      write(213,*)'  2'
      write(213,*)'  2'  
    else if (j==32) then ! SVID Ditch Diversion
      write(213,'(I4,A3,I3,A7,es10.2,A16)')j,'  1', seg(j,2),'  10  0', SFR_Flows(32),'  0  0  0  0.035'
      write(213,*)'  2'
      write(213,*)'  2'      
    end if
  enddo

  END SUBROUTINE  write_MODFLOW_SFR
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 SUBROUTINE write_SFR_template (im, nmonths, nSegs, SFR_Flows, drain_flow,SFR_Template)
! In this SUBROUTINE the input file for the SFR package is created.  
  INTEGER :: im, i, j, nmonths, nSegs,  SR_width, Trib_width
  INTEGER,dimension(32,2) :: seg
  REAL, DIMENSION (1:32) :: SFR_Flows
  CHARACTER(10) :: rough1,rough2,rough3
  CHARACTER(10)  ::  SFR_Template
  REAL, DIMENSION(nmonths) :: drain_flow
  
  seg(:,1) = (/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32/)    ! SFR Segment Number [nseg]
  seg(:,2) = (/3,3,4,5,9,8,8,9,10,12,12,14,14,23,17,17,19,19,22,26,22,23,25,25,26,30,29,29,30,0,0,0/)      ! SFR Inflow Segment [outseg]
  SR_width = 25
  Trib_width = 10
  rough1 = '@rough1  @'  ! Tailings
  rough2 = '@rough2  @'  ! Scott River
  rough3 = '@rough3  @'  ! Tributaies 

  if (SFR_Template=='UCODE') then
    open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')         
    write(214,'(I4,A12)')nSegs,'  0  0  0  0'
  else 
    open(unit=214, file='SVIHM_SFR.tpl',Access = 'append', status='old')         
    write(214,'(I4,A12)')nSegs,'  0  0  0  0'  
  end if                                  
                                                                                 
!  if (im==1) then                                                               ! Keep in case the beginning of the transient data section doesn't start printing on a new line 
!    open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')    ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,*)                                                                ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,'(I4,A12)')nSegs,'  0  0  0  0'                                   ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  else                                                                          ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  	open(unit=214, file='SVIHM_SFR.jtf',Access = 'append', status='old')     ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!    write(214,'(I4,A12)')nSegs,'  0  0  0  0'                                   ! Keep in case the beginning of the transient data section doesn't start printing on a new line
!  end if                                                                        ! Keep in case the beginning of the transient data section doesn't start printing on a new line
  
  do j = 1, nSegs
    if (j==1) then  ! EF + SF Inflow  	    
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0',SFR_Flows(1),'  0  0  0  ',rough1
      write(214,'(I4)')SR_width
      write(214,'(I4)')SR_width
    else if (j==2) then ! Sugar Creek Inflow 
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(2),'  0  0  0  ',rough1
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==3) then ! Last Tailings Segment 
      write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough1
      write(214,'(I4)')SR_width
      write(214,'(I4)')SR_width
    else if (j==4 .or. j==5 .or. j==9 .or. j==10 .or. j==12 .or. j==14 .or. j==23 .or. j==25 .or. j==26 .or. j==30) then ! Scott River Segments
  	  if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        else
      	  write(214,'(I3,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(214,'(I4,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        else
      	  write(214,'(I4,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough2
          write(214,'(I4)')SR_width
          write(214,'(I4)')SR_width
        end if
      end if
    else if (j==6) then ! French Creek Branch #1
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(6),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==7) then ! French Creek Branch #2	
      write(214,'(I3,A3,I3,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(7),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==8 .or. j==13 .or. j==17 .or. j==20 .or. j==22 .or. j==29) then ! Tributaries and Sloughs, Oro Fino Creek (no inflow)
      if (j .le. 9) then
      	if (seg(j,2) .le. 9) then
          write(214,'(I3,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        else
      	  write(214,'(I3,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        end if
      else
        if (seg(j,2) .le. 9) then
          write(214,'(I4,A3,I3,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        else
      	  write(214,'(I4,A3,I4,A24,A10)')j,'  1', seg(j,2),'  0  0.00E+00  0  0  0  ',rough3
          write(214,'(I4)')Trib_width
          write(214,'(I4)')Trib_width
        end if	
      end if
    else if (j==11) then ! Etna Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(11),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width	
    else if (j==15) then ! Johnson Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(15),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==16) then ! Crystal Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(16),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==18) then ! Patterson Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(18),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==19) then ! Big Slough Drain inflow segment
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', drain_flow(im),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==21) then ! Kidder Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(21),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==24) then ! Moffett Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(24),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==27) then ! Mill Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(27),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==28) then ! Shackleford Creek
      write(214,'(I4,A3,I4,A3,es10.2,A11,A10)')j,'  1', seg(j,2),'  0', SFR_Flows(28),'  0  0  0  ',rough3
      write(214,'(I4)')Trib_width
      write(214,'(I4)')Trib_width
    else if (j==31) then ! Farmers Ditch Diversion  
      write(214,'(I4,A3,I3,A6,es10.2,A16)')j,'  1', seg(j,2),'  3  0', SFR_Flows(31),'  0  0  0  0.035'
      write(214,*)'  2'
      write(214,*)'  2'  
    else if (j==32) then ! SVID Ditch Diversion
      write(214,'(I4,A3,I3,A7,es10.2,A16)')j,'  1', seg(j,2),'  10  0', SFR_Flows(32),'  0  0  0  0.035'
      write(214,*)'  2'
      write(214,*)'  2'      
    end if
  enddo
  END SUBROUTINE  write_SFR_template

END MODULE
