module m_read_main_input
  use m_file_io
  use m_error_handler, only: error_handler
  use m_options, only: t_options
!-------------------------------------------------------------------------------------------------!
! Module variables
  type(t_file_reader), pointer :: reader
  logical                      :: has_necessary_blocks(4) ! Discretization, Opt, Parameters, Inputs
!-------------------------------------------------------------------------------------------------!
  contains
!-------------------------------------------------------------------------------------------------!
  subroutine read_main_input(file, opt)
    implicit none
    character(100), intent(in)  :: file
    type(t_options), intent(in) :: opt
    integer                     :: eof_check
    character(100)              :: id
  
    reader => open_file_reader(file)
    
    eof_check = 0
    has_necessary_blocks = .false.
    do
      call reader%next_block_id(eof_check, id)
      if (eof_check == -1) exit
      call block_redirect(opt, id)
    end do
    call close_file(reader)
  
  end subroutine read_main_input
!-------------------------------------------------------------------------------------------------!
  subroutine block_redirect(opt, id)
    implicit none
    type(t_options), intent(in) :: opt
    character(*), intent(in)        :: id

    ! Regardless of block
    call pre_block_read_check(id)
    
    select case(trim(id))
      case("OPTIONS")
        ! This is the only block we let read it's own entries and set it's own read tracker (has_opt)
        call opt%read_options(reader)
      case("DISCRETIZATION")
        call read_DISCRETIZATION_block(reader)
      case("PARAMETERS")
        call read_PARAMETERS_block(reader)
      case("INPUTS")
      case("PRINT DAILY")
      case ("END")
        ! Do nothing - errant line? Block reader ended early?
      case DEFAULT
        call error_handler(1,reader%file,"Unknown Block Name: " // trim(id))
    end select

  end subroutine block_redirect
!-------------------------------------------------------------------------------------------------!
  
  subroutine pre_block_read_check(block_id)
    ! Generic hook for making sure a block has its prerequisite data read in.
    implicit none
    character(*), intent(in)        :: block_id
    
    select case(trim(block_id))
      case("THING")
        if (has_necessary_blocks(2)==.false.) call error_handler(2,reader%file,"ERROR")
      case DEFAULT
      ! Do nothing
    end select
  
  end subroutine pre_block_read_check
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine read_DISCRETIZATION_block(reader)
    use m_global, only: nmonths, WYstart, nrows, ncols, model_name, nSFR_inflow_segs
    use define_fields, only: npoly, nAgWells, nMuniWells, nlandcover
    use irrigation, only: nsubws
    implicit none
    type(t_file_reader), pointer    :: reader
    logical                         :: has_necessary_items(11) = .false.
    
    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings
    
    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("NMONTHS")
          nmonths = item2int(strings, 2)
          has_necessary_items(1) = .true.
        case("WYSTART")
          WYstart = item2int(strings, 2)
          has_necessary_items(2) = .true.
        case("NPOLY","NFIELDS")
          npoly = item2int(strings, 2)
          has_necessary_items(3) = .true.
        case("NSUBWS")
          nsubws = item2int(strings, 2)
          has_necessary_items(4) = .true.
        case("NLANDCOVER")
          nlandcover = item2int(strings, 2)
          has_necessary_items(5) = .true.
        case("NAGWELLS")
          nAgWells = item2int(strings, 2)
          has_necessary_items(6) = .true.
        case("NMUNIWELLS")
          nMuniWells = item2int(strings, 2)
          has_necessary_items(7) = .true.
        case("NSFR_INFLOW_SEGS")
          nSFR_inflow_segs = item2int(strings, 2)
          has_necessary_items(8) = .true.
        case("NAME","MFNAME")
          call item2char(strings, 2, model_name)
          has_necessary_items(9) = .true.
        case("NROWS")
          nrows = item2int(strings, 2)
          has_necessary_items(10) = .true.
        case("NCOLS")
          ncols = item2int(strings, 2)
          has_necessary_items(11) = .true.
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Discretization option: " // trim(id))
      end select
    end do
    ! Really simple input enforcement
    if (any(has_necessary_items==.false.)) then
      call error_handler(1,reader%file,"Missing items in Discretization Block")
    end if
  end subroutine read_DISCRETIZATION_block
  
!-------------------------------------------------------------------------------------------------!
  
  subroutine read_PARAMETERS_block(reader)
    use define_fields, only: RD_Mult
    implicit none
    type(t_file_reader), pointer    :: reader
    logical                         :: has_necessary_items(1) = .false.
    
    ! Standard file reader variables
    integer                    :: status, length
    character(30)              :: id, value
    type(t_vstringlist)        :: strings
    
    ! Loop until end of block or end of file
    do
      call reader%next_block_item(status, id, strings, length)
      if (status /= 0) exit  ! exit if end of block or end of file
      select case(trim(id))
        case("RD_MULT")
          RD_Mult = item2real(strings, 2)
          has_necessary_items(1) = .true.
        case DEFAULT
          call error_handler(1,reader%file,"Unknown Parameter: " // trim(id))
      end select
    end do
    ! Really simple input enforcement
    if (any(has_necessary_items==.false.)) then
      call error_handler(1,reader%file,"Missing items in Parameter Block")
    end if
    
    end subroutine read_PARAMETERS_block
    
!-------------------------------------------------------------------------------------------------!
end module m_read_main_input