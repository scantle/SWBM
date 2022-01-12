! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! SUBROUTINE IRRIGATION(ip, month, jday, eff_precip)
! 
!   INTEGER, INTENT(IN) :: ip, month, jday   
!   REAL, INTENT(IN) :: eff_precip
!   REAL             :: irreff_wl, irreff_cp
!    
!   if (sum(fields%irr_flag)>=250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true
!   
!   
!   select case (fields(ip)%SWBM_LU)
!     case (11)   ! alfalfa / grain
!       if(fields(ip)%rotation == 11) then                          ! Field is Alfalfa
!         daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
!         daily(ip)%pET=ETo*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU11
!         irreff_cp = irreff_cp_LU11
!         if ((month==6 .and. jday>=25 ) .or. (month>6)) then  ! If  March 25 - August 31
!           if ((daily(ip)%swc<(0.625*fields(ip)%whc)) .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 37.5% total soil swc storage, or after May 15th, or 20% of fields have started irrigating  
!             call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!           end if
!         end if
!       else if (fields(ip)%rotation == 12) then                ! Field is Grain
!         daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
!         daily(ip)%pET=ETo*Kc_grain*kc_grain_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU11
!         irreff_cp = irreff_cp_LU11
!         if ((month==6 .and. jday>=16 ) .or. (month>=7 .and. month<=9 ) .or. (month==10 .and. jday<=10)) then  ! If  March 16 - July 10
!           if ((daily(ip)%swc<(0.625*0.5*fields(ip)%whc)) &
!           .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 18.75% of total soil swc storage, or after May 15th, or 20% of fields have started irrigating	
!       	    call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!       	  end if
!       	end if
!       end if
!     case (2)    ! pasture
!         daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
!         daily(ip)%pET=ETo*Kc_pasture*kc_pasture_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU2
!         irreff_cp = irreff_cp_LU2
!         if ((month==7 .and. jday>=15 ) .or. (month>=8) .or. (month==0) .or. (month ==1 .and. jday<=15)) then  ! If  April 15 - October 15
!           if ((daily(ip)%swc<(0.45*0.5*fields(ip)%whc)) &
!            .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 77.5% total swc storage, or after May 15th, or 20% of fields have started irrigating
!             call IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!           end if
!         end if
!     case (3)    ! ET_noIRR
!     	daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
!       call ET_noIRR(month, jday, ip, eff_precip)
!     case (4)    ! noET_noIRR
!       daily(ip)%effprecip  = eff_precip                        ! Set effective precip 
!       call noET_noIRR (month, jday, ip)
!     case (6)    ! water SWBM_LU type
!   	  ! do nothing
!   end select
!   
! END SUBROUTINE IRRIGATION

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
!  SUBROUTINE IRRIGATION_RULESET(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!    
!    INTEGER, INTENT (IN) :: month, jday, ip
!    REAL, INTENT(IN)    :: irreff_wl, irreff_cp, eff_precip
!         
!    fields(ip)%irr_flag = 1  ! Field has started irrigating
!    if (fields(ip)%irr_type==1) then ! Flood irrigation
!      daily(ip)%tot_irr=max (0.,(1/irreff_flood )*(daily(ip)%pET-eff_precip))   
!      if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!      else if (fields(ip)%water_source==2) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==3) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
!    	daily(ip)%tot_irr=max (0.,(1/irreff_wl)*(daily(ip)%pET-eff_precip))   
!    	if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!     else if (fields(ip)%water_source==2) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==3) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==3) then ! Center Pivot irrigation
!      daily(ip)%tot_irr=max (0.,(1/irreff_cp)*(daily(ip)%pET-eff_precip))   
!      if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!      else if (fields(ip)%water_source==4) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==3) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==555) then ! Field has no water source
!      daily(ip)%tot_irr=0
!    end if          
!    return
!  END SUBROUTINE IRRIGATION_RULESET
!  
! ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! SUBROUTINE MAR(month,num_MAR_fields, MAR_fields, max_MAR_field_rate, MAR_vol, eff_precip, jday, moisture_save)
! 
!   INTEGER, INTENT(IN) :: num_MAR_fields, jday, month
!   INTEGER             :: iMAR
!   INTEGER, DIMENSION(num_MAR_fields), intent(in) :: MAR_fields
!   REAL, DIMENSION(num_MAR_fields), intent(in) :: max_MAR_field_rate
!   REAL, INTENT(in) :: MAR_vol
!   REAL, DIMENSION(npoly), intent(in) :: moisture_save
!   REAL :: eff_precip, rch
!   
!   daily%MAR            = 0.                                ! Reset daily MAR array (linear)
!   daily%MAR_vol        = 0.                                ! Reset daily MAR array (volumetric)
!   
!   if (month==4 .or. month==5 .or. month==6) then
!     do iMAR=1, num_MAR_fields
!       daily(MAR_fields(iMAR))%MAR = max_MAR_field_rate(iMAR)
!       daily(MAR_fields(iMAR))%MAR_vol = daily(MAR_fields(iMAR))%MAR * fields(MAR_fields(iMAR))%area
!       if (sum(daily%MAR_vol)>MAR_vol) then                                                            ! Don't exceed maximum availabe MAR volume per day
!         daily(MAR_fields(iMAR))%MAR_vol = 0.                                                          ! Reset MAR volume for field
!         daily(MAR_fields(iMAR))%MAR_vol = min(MAR_vol-sum(daily%MAR_vol),&
!                                               max_MAR_field_rate(iMAR)*fields(MAR_fields(iMAR))%area)   ! Minimum between available voume and max infiltration rate
!         daily(MAR_fields(iMAR))%MAR = daily(MAR_fields(iMAR))%MAR_vol / fields(MAR_fields(iMAR))%area   ! Convert volume to length      
!       end if
!       daily(MAR_fields(iMAR))%irrigation = daily(MAR_fields(iMAR))%irrigation + daily(MAR_fields(iMAR))%MAR ! Add MAR to existing irrigation value (irrigation only overlaps for 5 days in March so it should be relatively small)        
!       daily(MAR_fields(iMAR))%aET=min(daily(MAR_fields(iMAR))%pET,&
!                                            moisture_save(MAR_fields(iMAR))+eff_precip+&
!                                            daily(MAR_fields(iMAR))%irrigation) 
!       daily(MAR_fields(iMAR))%deficiency=daily(MAR_fields(iMAR))%pET-daily(MAR_fields(iMAR))%aET
!       if (daily(MAR_fields(iMAR))%aET > 0) daily(MAR_fields(iMAR))%ET_active =  1   ! Set ET flag to 1 if ET is active that day
!       if (fields(MAR_fields(iMAR))%SWBM_LU==2) then                                    ! if pasture  
!         rch = max(0., (moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
!                       -daily(MAR_fields(iMAR))%aET)-0.5*fields(MAR_fields(iMAR))%whc )
!         daily(MAR_fields(iMAR))%recharge = rch  
!       else if ( fields(MAR_fields(iMAR))%SWBM_LU==11 .or. fields(MAR_fields(iMAR))%SWBM_LU==3 )  then  ! if alfalfa/grain/native veg 
!         rch = max(0., (moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
!                       -daily(MAR_fields(iMAR))%aET)-fields(MAR_fields(iMAR))%whc )
!         daily(MAR_fields(iMAR))%recharge = rch 
!       else if (fields(MAR_fields(iMAR))%SWBM_LU==4) then  ! noET/NoIrr 
!         daily(MAR_fields(iMAR))%recharge = eff_precip
!       endif
!       daily(MAR_fields(iMAR))%swc=max(0.,moisture_save(MAR_fields(iMAR))+eff_precip+daily(MAR_fields(iMAR))%irrigation &
!                                              -daily(MAR_fields(iMAR))%aET-daily(MAR_fields(iMAR))%recharge)
!       
!       daily(MAR_fields(iMAR))%residual = daily(MAR_fields(iMAR))%swc-moisture_save(MAR_fields(iMAR)) &
!                                       +daily(MAR_fields(iMAR))%aET+daily(MAR_fields(iMAR))%recharge &
!                                       -eff_precip-daily(MAR_fields(iMAR))%irrigation 
!       previous(MAR_fields(iMAR))%swc = daily(MAR_fields(iMAR))%swc
!       daily(MAR_fields(iMAR))%change_in_storage = eff_precip+daily(MAR_fields(iMAR))%irrigation &
!                                                  -daily(MAR_fields(iMAR))%aET-daily(MAR_fields(iMAR))%recharge    
!     enddo
!   end if
! END SUBROUTINE MAR

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
!   SUBROUTINE IRRIGATION_ILR(ip, month, jday, eff_precip)
! 
!   INTEGER, INTENT(IN) :: ip, month, jday
!   REAL, INTENT(IN) :: eff_precip
!   REAL :: irreff_wl, irreff_cp
!   
!   if (sum(fields%irr_flag)>=250) irrigating = .true.      ! If 20% of the fields are irrigating (by number, not area; 1251 irrigated fields), set logical to true
!   if (sum(sw_irr) < sum(avail_sw_vol) .and. fields(ip)%ILR == .TRUE.) then
!     fields(ip)%ILR_Active = .true.                                                    
!   else if (sum(sw_irr) >= sum(avail_sw_vol) .or. fields(ip)%ILR == .FALSE.) then
!   	fields(ip)%ILR_Active = .false.
!   else
!   	write(*,*)'Invalid ILR Flag in Polygon Input File'
!   	write(*,*)'Value of ILR Flag = ',fields(ip)%ILR
!   	write(800,*)'Invalid ILR Flag in Polygon Input File'
!   	write(800,*)'Value of ILR Flag = ',fields(ip)%ILR
!   	call exit
!   end if
!   
!   select case (fields(ip)%SWBM_LU)
!     case (11)   ! alfalfa / grain
!       if(fields(ip)%rotation == 11) then  ! alfalfa
!       	daily(ip)%effprecip  = eff_precip                        ! Set effective precip
!         daily(ip)%pET=ETo*Kc_alfalfa*kc_alfalfa_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU11
!         irreff_cp = irreff_cp_LU11
!         if ((month==6 .and. jday>=25 ) .or. (month>6)) then  ! If  March 25 - August 31
!           if ((daily(ip)%swc<(0.625*fields(ip)%whc)) .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 37.5% total soil swc storage, or after May 15th, or 20% of fields have started irrigating  
!             call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!           end if
!         end if
!       else if (fields(ip)%rotation == 12) then  ! grain
!         daily(ip)%effprecip  = eff_precip                        ! Set effective precip
!         daily(ip)%pET=ETo*Kc_grain*kc_grain_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU11
!         irreff_cp = irreff_cp_LU11
!         if ((month==6 .and. jday>=16 ) .or. (month>=7 .and. month<=9 ) .or. (month==10 .and. jday<=10)) then  ! If  March 16 - July 10
!           if ((daily(ip)%swc<(0.625*0.5*fields(ip)%whc)) &
!           .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 18.75% of total soil swc storage, or after May 15th, or 20% of fields have started irrigating	
!       	    call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!       	  end if
!       	end if
!       end if
!     case (2)    ! pasture
!         daily(ip)%effprecip  = eff_precip                        ! Set effective precip
!         daily(ip)%pET=ETo*Kc_pasture*kc_pasture_mult  ! Set ET to current value for the day
!         irreff_wl = irreff_wl_LU2
!         irreff_cp = irreff_cp_LU2
!         if ((month==7 .and. jday>=15 ) .or. (month>=8) .or. (month==0) .or. (month ==1 .and. jday<=15)) then  ! If  April 15 - October 15
!           if ((daily(ip)%swc<(0.45*0.5*fields(ip)%whc)) &
!            .or. (month==8 .and. jday>=15) .or. (month>8) .or. irrigating) then  ! If soil swc is < 77.5% total swc storage, or after May 15th, or 20% of fields have started irrigating
!             call IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
!           end if
!         end if
!     case (3)    ! ET_noIRR
!       daily(ip)%effprecip  = eff_precip                        ! Set effective precip
!       call ET_noIRR(month, jday, ip, eff_precip)
!     case (4)    ! noET_noIRR
!     	daily(ip)%effprecip  = eff_precip                        ! Set effective precip
!       call noET_noIRR (month, jday, ip)
!     case (6)    ! water SWBM_LU type
!   	  ! do nothing
!   end select
!   
!   end subroutine IRRIGATION_ILR
!   
!   ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   
!   SUBROUTINE IRRIGATION_RULESET_ILR(month, jday, ip, irreff_wl, irreff_cp, eff_precip)
! 
!   INTEGER, INTENT(IN) :: month, jday, ip
!   REAL, INTENT(IN) :: eff_precip, irreff_wl, irreff_cp
! 
!   fields(ip)%irr_flag = 1
!   if (fields(ip)%ILR_Active) then  ! Irrigation ruleset for when ILR is active
! 	  if (fields(ip)%irr_type==1 .or. fields(ip)%irr_type==555) then ! Flood irrigation or DRY
! 	    daily(ip)%tot_irr=max (0.,(1/irreff_flood )*(daily(ip)%pET-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
! 	    if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!           sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!           surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if
!         if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	  sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!         	  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
!           end if              
!           daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
!         end if   
! 	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!         	sw_irr( fields(ip)%subws_ID ) = sw_irr( fields(ip)%subws_ID ) + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if 
!         if ( surfaceWater(subws)%sw_irr>avail_sw_vol(fields(ip)%subws_ID ) ) then
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!             sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!             surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           end if
!           daily(ip)%tot_irr=max (0.,(1/irreff_flood )*(daily(ip)%pET-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
!           daily(ip)%gw_irr = daily(ip)%tot_irr              
!         end if
! 	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
!         daily(ip)%tot_irr=0
!       end if
!     else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
!       daily(ip)%tot_irr=max (0.,(1/irreff_flood)*(daily(ip)%pET-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
!       if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!           sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!           surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if
!         if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	  sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!         	  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
!           end if              
!           daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
!         end if   
! 	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!         	sw_irr( fields(ip)%subws_ID ) = sw_irr( fields(ip)%subws_ID ) + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if 
!         if ( surfaceWater(subws)%sw_irr>avail_sw_vol(fields(ip)%subws_ID ) ) then
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!             sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!             surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           end if
!           daily(ip)%tot_irr=max (0.,(1/irreff_wl)*(daily(ip)%pET-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
!           daily(ip)%gw_irr = daily(ip)%tot_irr              
!         end if
! 	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
!         daily(ip)%tot_irr=0
!       end if
!     else if (fields(ip)%irr_type==3) then! Center pivot irrigation
!       daily(ip)%tot_irr=max (0.,(1/irreff_flood)*(daily(ip)%pET-eff_precip)*1.33)  ! Increase Irrigation by 33% (~ one additional irrigation)
!       if (fields(ip)%water_source==1 .or. fields(ip)%water_source==5) then  ! Surface-water or Dry
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!           sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!           surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if
!         if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	  sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!         	  surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplies have been exceeded
!           end if              
!           daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded 
!         end if   
! 	    else if (fields(ip)%water_source==2 .or. fields(ip)%water_source==3) then  ! Mixed or GW water source
! 	      if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!         	sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!           sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!         else
!         	sw_irr( fields(ip)%subws_ID ) = sw_irr( fields(ip)%subws_ID ) + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!         end if 
!         if ( surfaceWater(subws)%sw_irr>avail_sw_vol(fields(ip)%subws_ID ) ) then
!           if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!             sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!             sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           else
!             surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!           end if
!           daily(ip)%tot_irr=max (0.,(1/irreff_cp)*(daily(ip)%pET-eff_precip))  ! Convert irrigation rate back to normal (not increased by 33%)
!           daily(ip)%gw_irr = daily(ip)%tot_irr              
!         end if
! 	    else if (fields(ip)%water_source==4) then  ! Sub-irrigated 	
!         daily(ip)%tot_irr=0
!       end if
!     end if	
!   else ! Irrigation ruleset for when ILR is not active
!     if (fields(ip)%irr_type==1) then ! Flood irrigation
!      daily(ip)%tot_irr=max (0.,(1/irreff_flood )*(daily(ip)%pET-eff_precip))
!     if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!      else if (fields(ip)%water_source==2) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==3) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==2) then ! Wheel line irrigation
!    	daily(ip)%tot_irr=max (0.,(1/irreff_wl)*(daily(ip)%pET-eff_precip))   
!    	if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!      else if (fields(ip)%water_source==2) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==3) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==3) then ! Center Pivot irrigation
!      daily(ip)%tot_irr=max (0.,(1/irreff_cp)*(daily(ip)%pET-eff_precip))   
!      if (fields(ip)%water_source==1) then  ! Surface-water 
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%tot_irr = 0  ! Irrigation set to zero when surface-water supplies are exceeded
!        end if 
!      else if (fields(ip)%water_source==2) then  ! Groundwater
!        daily(ip)%gw_irr = daily(ip)%tot_irr  ! All irrigation assigned to groundwater well     
!      else if (fields(ip)%water_source==3) then  ! Mixed water source
!        if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!          sw_irr(1) = sw_irr(1) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!          sw_irr(9) = sw_irr(9) + daily(ip)%tot_irr * fields(ip)%area         ! Add daily irrigation to sw_irr counter
!        else
!          surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr + daily(ip)%tot_irr * fields(ip)%area  ! Add daily irrigation to sw_irr counter
!        end if
!        if (surfaceWater(subws)%sw_irr > avail_sw_vol(fields(ip)%subws_ID)) then 
!          if (fields(ip)%subws_ID == 1 .or. fields(ip)%subws_ID == 9) then ! Subwatersheds 1 and 9 both pull from EF+SF, so this stops double counting of water
!            sw_irr(1) = sw_irr(1) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!            sw_irr(9) = sw_irr(9) - daily(ip)%tot_irr * fields(ip)%area         ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          else
!            surfaceWater(subws)%sw_irr = surfaceWater(subws)%sw_irr - daily(ip)%tot_irr * fields(ip)%area  ! Revert Surface Water Irrgation Volume back to previous amount because surface water supplied have been exceeded
!          end if              
!          daily(ip)%gw_irr = daily(ip)%tot_irr ! Irrigation assigned to well when surface-water supplies are exceeded
!        end if
!      else if (fields(ip)%water_source==4 .or. fields(ip)%water_source==5 .or. fields(ip)%water_source==6) then  ! Sub-irrigated, dry, or water
!        daily(ip)%tot_irr=0
!      end if
!    else if (fields(ip)%irr_type==555) then ! Field has no water source
!      daily(ip)%tot_irr=0
!    end if 
!   end if    
! !  return
!   END SUBROUTINE IRRIGATION_RULESET_ILR

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~