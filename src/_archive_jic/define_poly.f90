MODULE define_poly

IMPLICIT NONE

type polygon
  INTEGER   :: SWBM_id
  INTEGER   :: SFR_seg
  INTEGER   :: landuse
  INTEGER   :: irr_type
  REAL      :: area
  INTEGER   :: well_id
  INTEGER   :: well_idx
  INTEGER   :: water_source
  INTEGER   :: num_MF_cells
  REAL      :: whc
  INTEGER   :: rotation
  REAL      :: av_recharge
  INTEGER   :: irr_flag
  INTEGER   :: WL2CP_year
  INTEGER   :: ILR_Flag
  LOGICAL   :: ILR_Active
  REAL      :: area_conv_fact
  REAL      :: MF_Area
  REAL      :: init_fill_frac
end type

type accumulator
	REAL      :: irrigation
	REAL      :: recharge
	REAL      :: well
	REAL      :: moisture
	REAL      :: evapotrasp
  REAL      :: actualET
  REAL      :: deficiency
  REAL      :: change_in_storage
  REAL      :: effprecip
  REAL      :: MAR
  REAL      :: budget
  INTEGER   :: daydef
  INTEGER   :: ET_active
  REAL      :: irrigation_vol
	REAL      :: recharge_vol
	REAL      :: well_vol
	REAL      :: moisture_vol
	REAL      :: evapotrasp_vol
  REAL      :: actualET_vol
  REAL      :: deficiency_vol
  REAL      :: change_in_storage_vol
  REAL      :: effprecip_vol
  REAL      :: MAR_vol
end type

type well
    INTEGER :: well_id, layer, well_row, well_col 
    REAL    :: coordx, coordy, monthly_vol, monthly_rate
    REAL    :: daily_well_vol, monthly_well_vol, monthly_well_rate
end type

INTEGER :: npoly, nrot, nwells, ip
REAL :: RD_Mult
type(polygon), allocatable, dimension(:) :: fields
type(accumulator), allocatable, dimension(:):: previous, monthly, daily, yearly
type (well), allocatable, dimension(:) :: ag_wells

contains

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE readpoly(npoly, nrows, ncols, output_zone_matrix)

  INTEGER, INTENT(IN):: npoly, nrows, ncols
  INTEGER, DIMENSION(nrows, ncols), INTENT(IN)  :: output_zone_matrix
  INTEGER :: i
  INTEGER, DIMENSION(nrows, ncols)  :: dummy_mat
  

  allocate(fields(npoly), monthly(npoly), daily(npoly), yearly(npoly), previous(npoly))
  daily%irrigation = 0.
  daily%daydef = 0

  open(unit=10,file="polygons_table.txt",status="old")
  read(10,*)       ! read headers into nothing
  nrot = 0
  write(800,*)'SWBM_ID SFR_Seg Landuse Irr_Type Area SWBM_2_MF_Con_Fact &
    &MF_Area Water_Src whc init_fill_frac WL2CP_year ILR_Flag' 
  do i=1, npoly
    read(10,*)fields(i)%SWBM_id, fields(i)%SFR_seg, fields(i)%landuse, &
      fields(i)%irr_type, fields(i)%area, fields(i)%water_source, fields(i)%whc, &
      fields(i)%init_fill_frac, fields(i)%WL2CP_year, fields(i)%ILR_Flag
    if (fields(i)%landuse==11 .and. fields(i)%water_source==1) fields(i)%water_source = 2   ! Alfalfa/Grain is never irrigated with surface-water only. Irrigation type is kept the same
    if (fields(i)%landuse == 11 .or. fields(i)%landuse == 3) then
      fields(i)%whc = fields(i)%whc * RD_Mult    ! Scale root zone depth by multiplier for alfalfa/grain and native veg (pasture fixed at 4 ft since whc is multiplied by 0.5 during the irrigation call)
    end if
    if ( fields(i)%landuse == 11 ) nrot = nrot + 1
    if (fields(i)%irr_type == 999) then 
      fields(i)%irr_type = 2                ! Change unknown irrigation type to wheel line
    endif
    if (fields(i)%irr_type == 555) then       ! Change non-irrigated field to dry irrigation type
      fields(i)%water_source = 5
    endif
    if (fields(i)%water_source == 999) then   ! Change unknown water source to groundwater
      fields(i)%water_source = 2
    endif
    dummy_mat = 0
    where (output_zone_matrix(:,:) == i) 
      dummy_mat(:,:) = 1
    end where
    fields(i)%num_MF_cells = SUM(dummy_mat)
    fields(i)%MF_area = fields(i)%num_MF_cells * 100 * 100
    fields(i)%area_conv_fact = fields(i)%MF_area / fields(i)%area
    write(800,'(i6,i3,i4,i5,f20.7,f20.7,f20.1,i5,f20.5,f20.5,i6,i3)')i ,fields(i)%SFR_seg,fields(i)%landuse, &
    fields(i)%irr_type, fields(i)%area, fields(i)%area_conv_fact, fields(i)%MF_Area, fields(i)%water_source, &
    fields(i)%whc, fields(i)%init_fill_frac, fields(i)%WL2CP_year, fields(i)%ILR_Flag
  enddo
  call initial_conditions
  close(10)
  write(*,*) nrot, " polygons do alfalfa/grain rotation"
  write(*,*) nwells, " irrigation wells"
  write(800,*)' '
  write(800,*) nrot, " polygons do alfalfa/grain rotation"
  write(800,*) nwells, " irrigation wells"
  write(800,*)' '

end subroutine readpoly

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE read_well(npoly, nwells)                                                                           
                                                                                               
  INTEGER, INTENT(IN) :: npoly, nwells
  INTEGER :: i, j, poly_id, well_id                                                                 
                                                                                                 
  allocate(ag_wells(nwells))
  open(unit=534, file="well_summary.txt", status="old")                                 
  read(534,*)                                                                                               
  do i=1, nwells                                                                          
    read(534,*)ag_wells(i)%well_id, ag_wells(i)%layer ,ag_wells(i)%well_row, ag_wells(i)%well_col, &
    ag_wells(i)%coordx, ag_wells(i)%coordy           
  enddo 
  close(534)
  
  fields%well_id = 0 
  open(unit=535, file="well_list_by_polygon.txt", status="old")                                                                                                                                                                                                                         
  read(535,*)  ! read header into nothing
  write(800,*)'SWBM_ID  Well_ID  WELL_IDX'
  do i = 1, npoly                                                                                                             
    read(535,*) poly_id, well_id                                                                     
    do j = 1, nwells                                                                      
      if (poly_id==fields(i)%SWBM_id) fields(i)%well_id = well_id
      if (well_id==ag_wells(j)%well_id) fields(i)%well_idx = j                             
    enddo
  write(800,'(3i6)') fields(i)%SWBM_id, fields(i)%well_id, fields(i)%well_idx 
  enddo                                                                                          
END subroutine read_well

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_month

    monthly%irrigation        = 0.         
    monthly%recharge          = 0.               
    monthly%well              = 0.           
    monthly%moisture          = 0.           
    monthly%evapotrasp        = 0.         
    monthly%actualET          = 0.          
    monthly%deficiency        = 0.      
    monthly%effprecip         = 0.          
    monthly%change_in_storage = 0.          
    monthly%MAR               = 0.  
    monthly%MAR_vol           = 0.            
    
end subroutine zero_month
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
subroutine zero_year
    
        yearly%irrigation        = 0.
        yearly%recharge          = 0.
        yearly%well              = 0.
        yearly%moisture          = 0.
        yearly%evapotrasp        = 0.
        yearly%actualET          = 0.
        yearly%deficiency        = 0. 
        yearly%effprecip         = 0. 
        yearly%change_in_storage = 0.
        yearly%MAR               = 0.
        yearly%MAR_vol           = 0.
    
end subroutine zero_year
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~    
subroutine initial_conditions

    previous%irrigation        = 0.
    previous%recharge          = 0.
    previous%well              = 0.
    previous%evapotrasp        = 0.
    previous%actualET          = 0.
    previous%deficiency        = 0.
    previous%effprecip         = 0.
    previous%change_in_storage = 0.
    previous%moisture = fields%whc * fields%init_fill_frac     ! Set previous day's moisture to same as initial condition
    daily%moisture  = fields%whc * fields%init_fill_frac     ! Set current day's moisture to initial condition
end subroutine initial_conditions
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     
subroutine Update_Irr_Type(im)
  
  integer :: ip, im, year
  
  year = im/12 + 1991     
  
  do ip = 1, npoly
     if (fields(ip)%WL2CP_year .LE. year .and. fields(ip)%WL2CP_year .NE. 0) then    ! If WL2CP Year
       fields(ip)%irr_type = 3                                                    ! Change irrigation type to center pivot
     end if
   enddo
   
    
    end subroutine Update_Irr_Type  
   
end MODULE
