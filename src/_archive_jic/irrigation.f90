MODULE irrigationmodule
  
  use define_poly
  implicit none
  
  REAL:: kc_grain, kc_alfalfa,kc_alfalfa_mult, kc_grain_mult, kc_noirr
  REAL:: kc_pasture, kc_pasture_mult
  REAL:: irreff_flood, irreff_wl_LU11, irreff_cp_LU11, irreff_wl_LU2, irreff_cp_LU2
  REAL :: AV_REF_ET_1a, AV_REF_ET_1b, AV_REF_ET_2, REF_ET
  REAL :: monthly_precip_vol
  REAL :: EF_SF_Ratio, Sugar_Ratio, Johnson_Ratio, Crystal_Ratio, Patterson_Ratio
  INTEGER, parameter:: nlanduse = 5
  LOGICAL :: irrigating
  INTEGER  :: nSegs, nInflowSegs, nDiv
  INTEGER, ALLOCATABLE, DIMENSION(:)   :: inflow_segs, div_segs
  REAL, ALLOCATABLE, DIMENSION(:)  :: seg_inflows, streamflow_in, SFR_flows, sw_irr, div_rate
  
  contains

  SUBROUTINE READ_KC_IRREFF 
  
    open(unit=10,file="irr_eff.txt",status="old")
    read(10,*)irreff_flood                     ! flood
    read(10,*)irreff_wl_LU11, irreff_cp_LU11   ! alfalfa/grain wheel line, alfalfa/grain center pivot
    read(10,*)irreff_wl_LU2, irreff_cp_LU2     ! pasture wheel line, pasture center pivot
    write(*,*) ! Blank Line
    write(*,'(A45)') "Effective Irrigation Efficiencies (IE + SMDF)"
    write(*,'(A8,F4.2)') "Flood = ", irreff_flood
    write(*,'(A22,F4.2)') "Alfalfa: Wheel Line = ", irreff_wl_LU11
    write(*,'(A24,F4.2)') "Alfalfa: Center Pivot = ", irreff_cp_LU11
    write(*,'(A22,F4.2)') "Pasture: Wheel Line = ", irreff_wl_LU2
    write(*,'(A24,F4.2)') "Pasture: Center Pivot = ", irreff_cp_LU2
    write(800,*) ! Blank Line
    write(800,'(A45)') "Effective Irrigation Efficiencies (IE + SMDF)"
    write(800,'(A8,F4.2)') "Flood = ", irreff_flood
    write(800,'(A22,F4.2)') "Alfalfa: Wheel Line = ", irreff_wl_LU11
    write(800,'(A24,F4.2)') "Alfalfa: Center Pivot = ", irreff_cp_LU11
    write(800,'(A22,F4.2)') "Pasture: Wheel Line = ", irreff_wl_LU2
    write(800,'(A24,F4.2)') "Pasture: Center Pivot = ", irreff_cp_LU2
    close (10)
    
    open(unit=11,file="crop_coeff_mult.txt",status="old")
    read(11,*)kc_alfalfa_mult, kc_grain_mult, kc_pasture_mult, kc_noirr
    close(11)
    write(*,*) ! Blank Line
    write(*,'(A18,F4.2)') "kc_alfalfa_mult = ", kc_alfalfa_mult
    write(*,'(A16,F4.2)') "kc_grain_mult = ", kc_grain_mult
    write(*,'(A18,F4.2)') "kc_pasture_mult = ", kc_pasture_mult
    write(*,'(A11,F4.2)') "kc_noirr = ", kc_noirr
    write(*,*) ! Blank Line
    write(800,*) ! Blank Line
    write(800,'(A24,F4.2)') "Alfalfa Kc Multiplier = ", kc_alfalfa_mult
    write(800,'(A22,F4.2)') "Grain Kc Multiplier = ", kc_grain_mult
    write(800,'(A24,F4.2)') "Pasture Kc Multiplier = ", kc_pasture_mult
    write(800,'(A23,F4.2)') "Native Vegetation Kc = ", kc_noirr
    write(800,*) ! Blank Line
    return
  end subroutine read_kc_irreff
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE IRRIGATION(ip, month, jday, eff_precip)

  INTEGER, INTENT(IN) :: ip, month, jday   
  REAL, INTENT(IN) :: eff_precip
  REAL             :: irreff_wl, irreff_cp
   
  if (sum(fields%irr_flag)>=250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true
  
  select case (fields(ip)%landuse)
    case (11)   ! alfalfa / grain
      if(fields(ip)%rotation == 11) then                          ! Field is Alfalfa
        daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
        daily(ip)%evapotrasp=REF_ET*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU11
        irreff_cp = irreff_cp_LU11
        if ((month==6 .and. jday>=25 ) .or. (month>6)) then  ! If  March 25 - August 31
          if ((daily(ip)%moisture<(0.625*fields(ip)%whc)) .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 37.5% total soil moisture storage, or after May 15th, or 20% of fields have started irrigating  
            call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
          end if
        end if
      else if (fields(ip)%rotation == 12) then                ! Field is Grain
        daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
        daily(ip)%evapotrasp=REF_ET*Kc_grain*kc_grain_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU11
        irreff_cp = irreff_cp_LU11
        if ((month==6 .and. jday>=16 ) .or. (month>=7 .and. month<=9 ) .or. (month==10 .and. jday<=10)) then  ! If  March 16 - July 10
          if ((daily(ip)%moisture<(0.625*0.5*fields(ip)%whc)) &
          .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 18.75% of total soil moisture storage, or after May 15th, or 20% of fields have started irrigating	
      	    call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
      	  end if
      	end if
      end if
    case (2)    ! pasture
        daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
        daily(ip)%evapotrasp=REF_ET*Kc_pasture*kc_pasture_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU2
        irreff_cp = irreff_cp_LU2
        if ((month==7 .and. jday>=15 ) .or. (month>=8) .or. (month==0) .or. (month ==1 .and. jday<=15)) then  ! If  April 15 - October 15
          if ((daily(ip)%moisture<(0.45*0.5*fields(ip)%whc)) &
           .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 77.5% total moisture storage, or after May 15th, or 20% of fields have started irrigating
            call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
          end if
        end if
    case (3)    ! ET_noIRR
    	daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
      call ET_noIRR(month, jday, ip, eff_precip)
    case (4)    ! noET_noIRR
      daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
      call noET_noIRR (month, jday, ip)
    case (6)    ! water landuse type
  	  ! do nothing
  end select
  
END SUBROUTINE IRRIGATION

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
 SUBROUTINE IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
   
   INTEGER, INTENT (IN) :: month, jday, ip
   REAL, INTENT(IN)    :: irreff_wl, irreff_cp, eff_precip
        
   fields(ip)%irr_flag = 1  ! Field has started irrigating
   if (fields(ip)%irr_type==1) then ! Flood irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-eff_precip))   
     if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (fields(ip)%water_source==2) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==3) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
   	daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-eff_precip))   
   	if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
    else if (fields(ip)%water_source==2) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==3) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==3) then ! Center Pivot irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_cp)*(daily(ip)%evapotrasp-eff_precip))   
     if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (fields(ip)%water_source==4) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==3) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==555) then ! Field has no water source
     daily(ip)%irrigation=0
   end if          
   return
 END SUBROUTINE IRRIGATION_RULESET
 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
SUBROUTINE MAR(month,num_MAR_fields, MAR_fields, max_MAR_field_rate, MAR_vol, eff_precip, jday, moisture_save)

  INTEGER, INTENT(IN) :: num_MAR_fields, jday, month
  INTEGER             :: iMAR
  INTEGER, DIMENSION(num_MAR_fields), intent(in) :: MAR_fields
  REAL, DIMENSION(num_MAR_fields), intent(in) :: max_MAR_field_rate
  REAL, INTENT(in) :: MAR_vol
  REAL, DIMENSION(npoly), intent(in) :: moisture_save
  REAL :: eff_precip, rch
  
  daily%MAR            = 0.                                ! Reset daily MAR array (linear)
  daily%MAR_vol        = 0.                                ! Reset daily MAR array (volumetric)
  
  if (month==4 .or. month==5 .or. month==6) then
    do iMAR=1, num_MAR_fields
      daily(MAR_fields(iMAR))%MAR = max_MAR_field_rate(iMAR)
      daily(MAR_fields(iMAR))%MAR_vol = daily(MAR_fields(iMAR))%MAR * fields(MAR_fields(iMAR))%area
      if (sum(daily%MAR_vol)>MAR_vol) then                                                            ! Don't exceed maximum availabe MAR volume per day
        daily(MAR_fields(iMAR))%MAR_vol = 0.                                                          ! Reset MAR volume for field
        daily(MAR_fields(iMAR))%MAR_vol = min(MAR_vol-sum(daily%MAR_vol),&
                                              max_MAR_field_rate(iMAR)*fields(MAR_fields(iMAR))%area)   ! Minimum between available voume and max infiltration rate
        daily(MAR_fields(iMAR))%MAR = daily(MAR_fields(iMAR))%MAR_vol / fields(MAR_fields(iMAR))%area   ! Convert volume to length      
      end if
      daily(MAR_fields(iMAR))%irrigation = daily(MAR_fields(iMAR))%irrigation + daily(MAR_fields(iMAR))%MAR ! Add MAR to existing irrigation value (irrigation only overlaps for 5 days in March so it should be relatively small)        
      daily(MAR_fields(iMAR))%actualET=min(daily(MAR_fields(iMAR))%evapotrasp,&
                                           moisture_save(MAR_fields(iMAR))+eff_precip+&
                                           daily(MAR_fields(iMAR))%irrigation) 
      daily(MAR_fields(iMAR))%deficiency=daily(MAR_fields(iMAR))%evapotrasp-daily(MAR_fields(iMAR))%actualET
      if (daily(MAR_fields(iMAR))%actualET > 0) daily(MAR_fields(iMAR))%ET_active =  1   ! Set ET flag to 1 if ET is active that day
      if (fields(MAR_fields(iMAR))%landuse==2) then                                    ! if pasture  
        rch = max(0., (moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
                      -daily(MAR_fields(iMAR))%actualET)-0.5*fields(MAR_fields(iMAR))%whc )
        daily(MAR_fields(iMAR))%recharge = rch  
      else if ( fields(MAR_fields(iMAR))%landuse==11 .or. fields(MAR_fields(iMAR))%landuse==3 )  then  ! if alfalfa/grain/native veg 
        rch = max(0., (moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
                      -daily(MAR_fields(iMAR))%actualET)-fields(MAR_fields(iMAR))%whc )
        daily(MAR_fields(iMAR))%recharge = rch 
      else if (fields(MAR_fields(iMAR))%landuse==4) then  ! noET/NoIrr 
        daily(MAR_fields(iMAR))%recharge = eff_precip
      endif
      daily(MAR_fields(iMAR))%moisture=max(0.,moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
                                             -daily(MAR_fields(iMAR))%actualET-daily(MAR_fields(iMAR))%recharge)
      
      daily(MAR_fields(iMAR))%budget = daily(MAR_fields(iMAR))%moisture-moisture_save(MAR_fields(iMAR)) &
                                      +daily(MAR_fields(iMAR))%actualET+daily(MAR_fields(iMAR))%recharge &
                                      -eff_precip-daily(MAR_fields(iMAR))%irrigation 
      previous(MAR_fields(iMAR))%moisture = daily(MAR_fields(iMAR))%moisture
      daily(MAR_fields(iMAR))%change_in_storage = eff_precip+daily(MAR_fields(iMAR))%irrigation &
                                                 -daily(MAR_fields(iMAR))%actualET-daily(MAR_fields(iMAR))%recharge    
    enddo
  end if
END SUBROUTINE MAR

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
  SUBROUTINE IRRIGATION_ILR(ip, month, jday, eff_precip)

  INTEGER, INTENT(IN) :: ip, month, jday
  REAL, INTENT(IN) :: eff_precip
  REAL :: irreff_wl, irreff_cp
  
  if (sum(fields%irr_flag)>=250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true
  if (sum(sw_irr) < sum(streamflow_in) .and. fields(ip)%ILR_Flag == 1) then
    fields(ip)%ILR_Active = .true.                                                    
  else if (sum(sw_irr) >= sum(streamflow_in) .or. fields(ip)%ILR_Flag == 0) then
  	fields(ip)%ILR_Active = .false.
  else
  	write(*,*)'Invalid ILR Flag in Polygon Input File'
  	write(*,*)'Value of ILR Flag = ',fields(ip)%ILR_Flag
  	write(800,*)'Invalid ILR Flag in Polygon Input File'
  	write(800,*)'Value of ILR Flag = ',fields(ip)%ILR_Flag
  	call exit
  end if
  
  select case (fields(ip)%landuse)
    case (11)   ! alfalfa / grain
      if(fields(ip)%rotation == 11) then  ! alfalfa
      	daily(ip)%effprecip  = eff_precip                        ! Set effective precip
        daily(ip)%evapotrasp=REF_ET*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU11
        irreff_cp = irreff_cp_LU11
        if ((month==6 .and. jday>=25 ) .or. (month>6)) then  ! If  March 25 - August 31
          if ((daily(ip)%moisture<(0.625*fields(ip)%whc)) .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 37.5% total soil moisture storage, or after May 15th, or 20% of fields have started irrigating  
            call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
          end if
        end if
      else if (fields(ip)%rotation == 12) then  ! grain
        daily(ip)%effprecip  = eff_precip                        ! Set effective precip
        daily(ip)%evapotrasp=REF_ET*Kc_grain*kc_grain_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU11
        irreff_cp = irreff_cp_LU11
        if ((month==6 .and. jday>=16 ) .or. (month>=7 .and. month<=9 ) .or. (month==10 .and. jday<=10)) then  ! If  March 16 - July 10
          if ((daily(ip)%moisture<(0.625*0.5*fields(ip)%whc)) &
          .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 18.75% of total soil moisture storage, or after May 15th, or 20% of fields have started irrigating	
      	    call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
      	  end if
      	end if
      end if
    case (2)    ! pasture
        daily(ip)%effprecip  = eff_precip                        ! Set effective precip
        daily(ip)%evapotrasp=REF_ET*Kc_pasture*kc_pasture_mult  ! Set ET to current value for the day
        irreff_wl = irreff_wl_LU2
        irreff_cp = irreff_cp_LU2
        if ((month==7 .and. jday>=15 ) .or. (month>=8) .or. (month==0) .or. (month ==1 .and. jday<=15)) then  ! If  April 15 - October 15
          if ((daily(ip)%moisture<(0.45*0.5*fields(ip)%whc)) &
           .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil moisture is < 77.5% total moisture storage, or after May 15th, or 20% of fields have started irrigating
            call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
          end if
        end if
    case (3)    ! ET_noIRR
      daily(ip)%effprecip  = eff_precip                        ! Set effective precip
      call ET_noIRR(month, jday, ip, eff_precip)
    case (4)    ! noET_noIRR
    	daily(ip)%effprecip  = eff_precip                        ! Set effective precip
      call noET_noIRR (month, jday, ip)
    case (6)    ! water landuse type
  	  ! do nothing
  end select
  
  end subroutine IRRIGATION_ILR
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)

  INTEGER, INTENT(IN) :: month, jday, ip
  REAL, INTENT(IN) :: eff_precip, irreff_wl, irreff_cp

  fields(ip)%irr_flag = 1
  if (fields(ip)%ILR_Active) then  ! Irrigation ruleset for when ILR is active
	  if (fields(ip)%irr_type==1 .or. fields(ip)%irr_type==555) then ! Flood irrigation or DRY
	    daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
	    if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( fields(ip)%SFR_seg ) = sw_irr( fields(ip)%SFR_seg ) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(fields(ip)%SFR_seg)>streamflow_in(fields(ip)%SFR_seg ) ) then
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
      daily(ip)%irrigation=max (0.,(1/irreff_flood)*(daily(ip)%evapotrasp-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
      if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( fields(ip)%SFR_seg ) = sw_irr( fields(ip)%SFR_seg ) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(fields(ip)%SFR_seg)>streamflow_in(fields(ip)%SFR_seg ) ) then
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    else if (fields(ip)%irr_type==3) then! Center pivot irrigation
      daily(ip)%irrigation=max (0.,(1/irreff_flood)*(daily(ip)%evapotrasp-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
      if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
          sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
          sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if
        if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	  sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
        	  sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
          end if              
          daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
        end if   
	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
	      if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
        	sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
          sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
        else
        	sw_irr( fields(ip)%SFR_seg ) = sw_irr( fields(ip)%SFR_seg ) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
        end if 
        if ( sw_irr(fields(ip)%SFR_seg)>streamflow_in(fields(ip)%SFR_seg ) ) then
          if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
            sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
            sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          else
            sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
          end if
          daily(ip)%irrigation=max (0.,(1/irreff_cp)*(daily(ip)%evapotrasp-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
          daily(ip)%well = daily(ip)%irrigation              
        end if
	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
        daily(ip)%irrigation=0
      end if
    end if	
  else ! Irrigation ruleset for when ILR is not active
    if (fields(ip)%irr_type==1) then ! Flood irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_flood )*(daily(ip)%evapotrasp-eff_precip))
    if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (fields(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==3) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
   	daily(ip)%irrigation=max (0.,(1/irreff_wl)*(daily(ip)%evapotrasp-eff_precip))   
   	if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (fields(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==3) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==3) then ! Center Pivot irrigation
     daily(ip)%irrigation=max (0.,(1/irreff_cp)*(daily(ip)%evapotrasp-eff_precip))   
     if (fields(ip)%water_source==1) then  ! Surface-water 
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%irrigation = 0  ! Irrigation set to zero when surface-water supplies are exceeded
       end if 
     else if (fields(ip)%water_source==2) then  ! Groundwater
       daily(ip)%well = daily(ip)%irrigation  ! All irrigation assigned to groundwater well     
     else if (fields(ip)%water_source==3) then  ! Mixed water source
       if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
         sw_irr(1) = sw_irr(1) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
         sw_irr(9) = sw_irr(9) + daily(ip)%irrigation * fields(ip)%MF_area         ! Add daily irrigation to sw_irr counter
       else
         sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) + daily(ip)%irrigation * fields(ip)%MF_area  ! Add daily irrigation to sw_irr counter
       end if
       if (sw_irr(fields(ip)%SFR_seg) > streamflow_in(fields(ip)%SFR_seg)) then 
         if (fields(ip)%SFR_seg == 1 .or. fields(ip)%SFR_seg == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
           sw_irr(1) = sw_irr(1) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
           sw_irr(9) = sw_irr(9) - daily(ip)%irrigation * fields(ip)%MF_area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         else
           sw_irr(fields(ip)%SFR_seg) = sw_irr(fields(ip)%SFR_seg) - daily(ip)%irrigation * fields(ip)%MF_area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
         end if              
         daily(ip)%well = daily(ip)%irrigation ! Irrigation assigned to well when surface-water supplies are exceeded
       end if
     else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
       daily(ip)%irrigation=0
     end if
   else if (fields(ip)%irr_type==555) then ! Field has no water source
     daily(ip)%irrigation=0
   end if 
  end if    
!  return
  END SUBROUTINE IRRIGATION_RULESET_ILR

!******************************************************************************************************************************************

  SUBROUTINE ET_noIRR(month, jday, ip, eff_precip) 
  
  INTEGER, INTENT(IN) :: month, jday, ip
  REAL, INTENT(IN)  ::  eff_precip
                                                       
   daily(ip)%irrigation= 0.
   daily(ip)%well=0.
   daily(ip)%evapotrasp=kc_noirr*REF_ET
   if (daily(ip)%evapotrasp >= (previous(ip)%moisture+eff_precip)) then 
	   daily(ip)%evapotrasp = previous(ip)%moisture+eff_precip
	 end if

  end subroutine ET_noIRR

!*************************************************************************************************************************************

  SUBROUTINE noET_noIRR(month, jday, ip)
  ! Since the tailings are a much larger percentage of this category than the urban areas (e.g., Etna), any 
  ! precipitation is assumed to go to groundwater recharge since there is limited storage due to the coarse material.
  ! There should be little to no recharge in the impermeable urban areas, but they are a much smaller proportion 
  ! compared with the tailings so we have neglected them for now. In the future it may be advisable to split the
  ! noET_noIrr category to urban and tailings.
    INTEGER :: month, jday, ip
       
    daily(ip)%irrigation=0.
    daily(ip)%well=0.      
    daily(ip)%moisture=0.  
    daily(ip)%evapotrasp=0.
                                      
  end SUBROUTINE noET_noIRR

 ! *************************************************************************************************************************************
  subroutine do_rotation(im)

    INTEGER, INTENT(IN) :: im
    INTEGER :: rotcycle, ipr, year, ngrain, i
  
    ngrain = nrot/8
    year = im/12 + 1 
    rotcycle=mod(year,8)

    ipr = 0
    print*, ''
    write(*,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
    write(*,*)'-------------------------------------------------------------'
    write(800,*) ''
    write(800,'(a5,i2,a26,2i4)')"Year:", year,"   Grain Polygon ID Range:", rotcycle*ngrain+1, (rotcycle+1)*ngrain
    write(800,*)'-------------------------------------------------------------'
    do i = 1, npoly
      if (fields(i)%landuse == 11) then    
        ipr = ipr + 1
        fields(i)%rotation = 11 ! alfalfa
        if ( ipr>(rotcycle*ngrain) .and. ipr<=((rotcycle+1)*ngrain) ) then
          fields(i)%rotation = 12
        end if
      end if
    enddo

  end subroutine do_rotation       
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
 
subroutine initialize_streams

  INTEGER :: dummy, i
  
  open(unit=213, file='SFR_network.txt', status='old')         
  open(unit=214, file='SFR_inflow_segments.txt', status='old') 
  open(unit = 215, file = 'SFR_inflows.txt', status = 'old')   
  open(unit = 216, file = 'SFR_diversions.txt', status = 'old')
  read(213,*) dummy, nSegs 
  close(213)
  ALLOCATE(streamflow_in(nSegs))
  ALLOCATE(SFR_flows(nSegs))
  read(214,*) nInflowSegs
  ALLOCATE(inflow_segs(nInflowSegs))
  ALLOCATE(seg_inflows(nInflowSegs))
  ALLOCATE(sw_irr(nInflowSegs))
	DO i=1, nInflowSegs
	  read(214,*) inflow_segs(i)
  enddo
  read(215,*) ! Read header into nothing
  read(216,*) nDiv
  ALLOCATE(div_segs(nDiv))
  ALLOCATE(div_rate(nDiv))                                               
  read(216,*) (div_segs(i), i=1, nDiv)
  
end subroutine initialize_streams

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine read_monthly_stream_inflow(numdays)
    
    INTEGER, INTENT(IN) :: numdays
    INTEGER :: i
    CHARACTER(10) :: date_dummy
    
    seg_inflows(:) = 0.                                          ! Reset all segment inflows to zero
    sw_irr(:) = 0.                                               ! Reset surface-water irrigation to zero        
    streamflow_in(:) = 0.                                        ! Reset all stream inflows to zero
    read(215,*) date_dummy, (seg_inflows(i), i=1, nInflowSegs)   ! Read in date and average monthly inflow rate for each stream simulated
    streamflow_in(inflow_segs) = seg_inflows * numdays           ! Convert average monthly inflow rate to volume
        
  end subroutine read_monthly_stream_inflow
  
  ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  subroutine SFR_streamflow(numdays)
    
    INTEGER, INTENT(IN) :: numdays
    INTEGER :: i
    
    SFR_flows(:) = 0.                                       ! Reset all SFR flow to zero
    SFR_flows(inflow_segs) = (seg_inflows - sw_irr)/numdays ! Subtract SW irrigation from inflows to get SFR in
    div_rate(:)= 0.                                              ! Resent diversion rates to zero
    read(216,*) (div_rate(i), i=1, nDiv)                         ! Read in diversions
    SFR_flows(div_segs) = div_rate                          ! Assign diversions to segments
    
  end subroutine SFR_streamflow
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ! Likely don't need anymore with re-code of SFR_streamflow, but maybe keep and include an addition input file for MAR diversions so the original diversion file can be re-used (?) 
  
  ! subroutine SFR_streamflow_w_MAR(numdays)
  !   
  !   INTEGER :: numdays    
  !   
  !   SFR_Flows = 0.            ! Reset all SFR flows to zero
  !   SFR_streamflow(:) = streamflow_in(:) - sw_irr(:)
  !   
  !   SFR_Flows(1)  = (SFR_streamflow(1) * EF_SF_Ratio) / numdays     ! EF + SF Inflow  	        
  !   SFR_Flows(2)  = (SFR_streamflow(1) * Sugar_Ratio) / numdays     ! Sugar Creek Inflow    
  !   SFR_Flows(6)  = (SFR_streamflow(2) * 0.5) / numdays              ! French Creek Branch #1
  !   SFR_Flows(7)  = (SFR_streamflow(2) * 0.5) / numdays              ! French Creek Branch #2
  !   SFR_Flows(11) =  SFR_streamflow(3)  / numdays                   ! Etna Creek           
  !   SFR_Flows(15) = (SFR_streamflow(4) * Johnson_Ratio) / numdays   ! Johnson Creek        
  !   SFR_Flows(16) = (SFR_streamflow(4) * Crystal_Ratio) / numdays   ! Crystal Creek        
  !   SFR_Flows(18) = (SFR_streamflow(4) * Patterson_Ratio) / numdays ! Patterson Creek      
  !   SFR_Flows(21) =  SFR_streamflow(5) / numdays                   ! Kidder Creek        
  !   SFR_Flows(24) =  SFR_streamflow(6) / numdays                    ! Moffett Creek        
  !   SFR_Flows(27) =  SFR_streamflow(7) / numdays                    ! Mill Creek           
  !   SFR_Flows(28) =  SFR_streamflow(8) / numdays                    ! Shackleford Creek    
  !   if (month == 0 .or. month == 1 .or. month == 2 .or. month == 3 .or. month == 11) then ! Ditches only active from April - July        
  !     SFR_Flows(31) = 0.
  !     SFR_Flows(32) = 0.
  !   else if (month == 4 .or. month == 5 .or. month == 6) then
  !     SFR_Flows(31) = 0.                                            ! No MAR Diversion from Farmer's Ditch
  !     SFR_Flows(32) = sum(monthly%MAR_vol) / numdays                ! Divert MAR volume for the month converted to a daily flow rate
  !   else
  !     SFR_Flows(31) = 8.  * 2446.58                                 ! Farmers Ditch Diversion (~8 cfs total diversion, leakage rate is about 6 cfs, assumed 2 cfs consumptive use)
  !     SFR_Flows(32) = 16. * 2446.58                                 ! SVID Diversion (~16 cfs total diversion, leakage rate is about 14 cfs, assumed 2 cfs consumptive use)     	
  !   end if
  ! end subroutine SFR_streamflow_w_MAR
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    

  SUBROUTINE deficiency_check(ip,month,iday)

  INTEGER, INTENT(IN) :: ip, month, iday
  INTEGER, SAVE :: year = 0

  if ((fields(ip)%rotation == 12 .or. fields(ip)%landuse  == 2) .and. (0.5*fields(ip)%whc.ne.0.)) then    ! if grain or pasture

    if ( (month == 4).and. (iday==1) ) then
       daily(ip)%daydef = 0
       if (ip==1) year = year + 1
    endif
    if (year == 0 ) return

    ! if ip has already had positive deficiency for > 5 days, do nothing
    if (daily(ip)%daydef < 0 ) return
    
    ! otherwise...
    
    if (daily(ip)%deficiency>0) then
        daily(ip)%daydef = daily(ip)%daydef + 1
    else
        daily(ip)%daydef = 0
    endif
    
    if (daily(ip)%daydef>4) then
      write(532,*) 'fields',ip,'date', iday, month, year
      daily(ip)%daydef = -100
    endif  
  
  elseif ((fields(ip)%rotation == 11 .or. fields(ip)%landuse  ==3) .and. (fields(ip)%whc.ne.0.)) then  ! if alfalfa or native veg    

  if ( (month == 4).and. (iday==1) ) then
     daily(ip)%daydef = 0
     if (ip==1) year = year + 1
  endif
  if (year == 0 ) return

  ! if ip has already had positive deficiency for > 5 days, do nothing
  if (daily(ip)%daydef < 0 ) return

  ! otherwise...

  if (daily(ip)%deficiency>0) then
      daily(ip)%daydef = daily(ip)%daydef + 1
  else
      daily(ip)%daydef = 0
  endif
  
  if (daily(ip)%daydef>4) then
    write(532,*) 'fields',ip,'date', iday, month, year
    daily(ip)%daydef = -100
  endif  
  
  endif

  END SUBROUTINE deficiency_check

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE RECHARGE(ip,eff_precip,jday,month,moisture_save,MAR_active)

!  use define_poly

  INTEGER, INTENT (IN) :: ip, jday, month
  REAL, INTENT (IN) :: eff_precip
  REAL, DIMENSION(npoly), INTENT(inout) :: moisture_save
  LOGICAL, INTENT(in)  :: MAR_active
  REAL:: rch
  
  if (fields(ip)%landuse /= 6) then      ! No recharge for fields with water landuse
    daily(ip)%actualET=min(daily(ip)%evapotrasp,previous(ip)%moisture+eff_precip+daily(ip)%irrigation) 
    daily(ip)%deficiency=daily(ip)%evapotrasp-daily(ip)%actualET
    if (daily(ip)%actualET > 0) daily(ip)%ET_active =  1   ! Set ET flag to 1 if ET is active that day
    if (fields(ip)%landuse==2) then                                    ! if pasture  
      rch = max(0., (previous(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET)-0.5*fields(ip)%whc )
      daily(ip)%recharge = rch  
    else if ( fields(ip)%landuse==11 .or. fields(ip)%landuse==3 )  then  ! if alfalfa/grain/native veg 
      rch = max(0., (previous(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET)-fields(ip)%whc )
      daily(ip)%recharge = rch 
    else if (fields(ip)%landuse==4) then  ! noET/NoIrr 
      daily(ip)%recharge = eff_precip
    endif
    daily(ip)%moisture=max(0.,previous(ip)%moisture+eff_precip+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge)
    CALL waterbudget(ip, eff_precip)
    if (MAR_active) moisture_save(ip) = previous(ip)%moisture 
    previous(ip)%moisture = daily(ip)%moisture
    daily(ip)%change_in_storage = eff_precip+daily(ip)%irrigation-daily(ip)%actualET-daily(ip)%recharge	
  end if  

  END SUBROUTINE RECHARGE
  
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  SUBROUTINE waterbudget(ip, eff_precip)

  INTEGER, INTENT (IN) :: ip
  REAL, INTENT(IN)    :: eff_precip
  
  daily(ip)%budget = daily(ip)%moisture-previous(ip)%moisture+daily(ip)%actualET+daily(ip)%recharge- &
                     eff_precip-daily(ip)%irrigation       

  END SUBROUTINE 

END MODULE