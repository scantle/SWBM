module water_mover
  use m_error_handler, only: error_handler
  use m_options, only: t_options
!-------------------------------------------------------------------------------------------------!
! Module variables
  integer                  :: nmoves
  integer, allocatable     :: from_to(:,:)   ! Move water from index/to index ([from,to], nmoves)
  integer, allocatable     :: move_type(:,:) ! ([from,to], nmoves) 1-Well 2-SFR 3-MAR
  integer, allocatable     :: move_date(:,:) ! start SP, end SP ([start,end], nmoves)
  real, allocatable        :: sfr_rate(:)    ! m3/day (nreaches)
  real, allocatable        :: wel_rate(:)    ! m3/day (nwells)
  real, allocatable        :: mar_rate(:)    ! m3/day (nfields)
    
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
  subroutine read_water_mover_input_file(filename)
    use m_file_io
    use m_vstringlist, only: t_vstringlist
    use irrigation, only: nsegs
    use define_fields, only: npoly, nAgWells
    implicit none
    character(*), intent(in)     :: filename
    type(t_file_reader), pointer :: reader
    integer                      :: i
    character(10)                :: temp
    real                         :: rate
    
    ! Standard file reader variables
    integer                    :: status, length, eof
    character(30)              :: id, value
    type(t_vstringlist)        :: strings
    
    reader => open_file_reader(filename)
    
    call reader%next_block_id(eof, id)
    select case(trim(id))
      case("WATER_MOVER")  ! Should be the only thing in this file!
        nmoves = reader%get_block_len()
        ! Know everything we need to allocate
        allocate(from_to(2,nmoves), move_type(2,nmoves), move_date(2,nmoves))
        allocate(sfr_rate(nsegs), wel_rate(nAgWElls), mar_rate(npoly))
        from_to   = 0
        move_type = 0
        move_date = 0
        sfr_rate  = 0.0
        wel_rate  = 0.0
        mar_rate  = 0.0
        
        ! Read lines
        do i=1, nmoves
          call reader%next_block_item(status, id, strings, length)
          move_type(1,i) = find_string_index_in_list(strings, 1, "WELL,SFR", toupper=.true.)
          from_to(1,i)   = item2int(strings, 2)
          move_type(2,i) = find_string_index_in_list(strings, 3, "WELL,SFR,MAR", toupper=.true.)
          from_to(2,i)   = item2int(strings, 4)
          move_date(1,i) = item2int(strings, 5)
          move_date(1,i) = item2int(strings, 6)
          rate           = item2real(strings,7)
          
          ! Handle Move FROM
          select case(move_type(1,i))
            case(1) ! WELL
              if ((from_to(1,i) > 0) .and. (from_to(1,i) <= nAgWells)) then
                wel_rate(from_to(1,i)) = -1 * rate
              else
                call item2char(strings, 2, temp)
                call error_handler(1,reader%file,"Invalid from well id, check nAgWells. well id = "//temp)
              end if
            case(2) ! SFR
              if (from_to(1,i) > 0 .and. from_to(1,i) <= nsegs) then
                sfr_rate(from_to(1,i)) = -1 * rate
              else
                call item2char(strings, 2, temp)
                call error_handler(1,reader%file,"Invalid from segment id = "//temp)
              end if
            case DEFAULT
              call error_handler(1,reader%file,"Invalid from destination: Must be one of WELL, SFR")
          end select
          
          ! Handle Move TO
          select case(move_type(2,i))
            case(1) ! WELL
              if (from_to(2,i) > 0 .or. from_to(2,i) <= nAgWells) then
                wel_rate(from_to(1,i)) = rate
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid from well id, check nAgWells. well id = "//temp)
              end if
            case(2) ! SFR
              if (from_to(2,i) > 0 .or. from_to(2,i) <= nsegs) then
                sfr_rate(from_to(2,i)) = rate
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid from segment id = "//temp)
              end if
            case(3) ! MAR
              if (from_to(2,i) > 0 .or. from_to(2,i) <= npoly) then
                mar_rate(from_to(2,i)) = rate
              else
                call item2char(strings, 4, temp)
                call error_handler(1,reader%file,"Invalid Polygon/Field id = "//temp)
              end if
            case DEFAULT
              call error_handler(1,reader%file,"Bad from destination: Must be one of WELL, SFR")
          end select
          
        end do
        
      case DEFAULT
        call error_handler(1,reader%file,"Invalid Block: " // trim(id))
    end select
  
  end subroutine read_water_mover_input_file
!-------------------------------------------------------------------------------------------------!
end module water_mover