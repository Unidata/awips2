C$PRAGMA C (get_apps_defaults)
ckwz C$PRAGMA C (GET_USER)
C$PRAGMA C (EXECUTE58)
C ex58.f
C
C Execution routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C PO    I       a real array, internally dimensioned PO(1)
C               contains nothing for RES-J
C CO    I       a real array, internally dimensioned CO(1)
C               contains carryover values for RES-J
C               operation 
C
       subroutine ex58( po, co, d, to )

C    DEBUG INFO
      INCLUDE 'common/fdbug'
C
C     INTEGER IODBUG            ! Unit number ofr debug printout.
C     INTEGER ITRACE            ! Trace level.
C     INTEGER IDBALL            ! debug info requested for all ops?
C     INTEGER NDEBUG            ! number of ops with debug requested
C     INTEGER IDEBUG(20)        ! List of ops with debug requested

C    RUN PERIOD INFORMATION
      INCLUDE 'common/fctime'
C     INTEGER IDA               ! Julian date of the first day to be 
C                               ! computed in the current pass though 
C                               ! the operations table
C
C     INTEGER IHR               ! The first hour to be computed in the 
C                               ! current pass though the operations table
C
C     INTEGER LDA               ! Julian date of the last day to be 
C                               ! computed in the current pass though 
C                               ! the operations table
C
C     INTEGER LHR               ! The last hour to be computed in the 
C                               ! current pass though the operations table
C
C     INTEGER IDADAT            ! The Julian date of the first day of time
C                               ! time series data stored in the D array

C    MAIN PROGRAM TYPE
      INCLUDE 'common/fprog'
C     INTEGER   MAINUM          ! Main program code
C                               ! 1 = ofs, 2 = esp, 3 = mcp3, 4 = opt3
C     REAL      VERS            ! Version number
C     REAL      VDATE(2)        ! Version date
C     REAL      PNAME(5)        ! program name
C     INTEGER   NDD             ! Number of days per time series in D array

C    RFS IO UNIT NUMBERS
      INCLUDE 'common/ionum'
C     INTEGER IN                ! Unit number of standard input device
C     INTEGER IPR               ! Unit number of standard print file
C     INTEGER IPU               ! Unit number of punch device


C STORING OF CARRYOVER INFORMATION
      INCLUDE 'common/fcary'

C     INTEGER IFILLC 
C     INTEGER NCSTOR
C     INTEGER ICDAY(20)
C     INTEGER ICHOUR(20)

C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
C     INTEGER ISEG(2)           ! Identifier for segment
C     INTEGER IOPNUM            ! Operation number currently executed
C     INTEGER OPNAME(2)         ! 8 character name of operation currently being executed

      INCLUDE 'common/cdate'

      INTEGER ibug, iusec, irstat, d_index(100)
      INTEGER idt
      REAL r_id(2)
      CHARACTER         ch_po(4)              !!! get
      CHARACTER*8       oname
      REAL              r_po
      EQUIVALENCE       (ch_po(1), r_po)        !!! used to get time series info out of po array

      
CCC  what's this!!!!   $HP9000_800 INTRINSICS
 

C    DATA
      data  oname / 'RES-J   ' /

C    LOCAL VARIABLES
      REAL po(*)                ! Real array containing the parameteric data
      REAL  co(*), d(*)
      INTEGER to(*)
      CHARACTER*128 resjFile    ! File name of RES-J fs5file
C      CHARACTER*128    mybin   ! character string to hold name of mybin dirctories 
ckwz      CHARACTER*128     user    ! character string to hold name of user
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
ckwz change user to UserHome for the TEMP.RESJ file

C    LOCAL VARIABLES
      
      INTEGER   ierr            ! 0 if no error
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/ex58.f,v $
     . $',                                                             '
     .$Id: ex58.f,v 1.9 2004/10/08 19:23:58 hank Exp $
     . $' /
C    ===================================================================
C

C    DEBUG INITIALIZATION

      ierr = 0
      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** ENTER ex58'
      endif
      CALL FOPCDE ( oname, iex58 )
      CALL FPRBUG ( oname, 1, iex58, ibug )

C    GET THE NAME OF THE RES-J TEMP FILE
      if ( (MAINUM .EQ. 1) .OR. (MAINUM.EQ.2) ) then                 !!! ofs + esp
        call generateRESJFileName( po, resjFile, iseg, ierr )
          if ( ierr .GT. 0 ) then
            write ( ipr, 101 )
101         FORMAT ( 1H0, 10X, 10H**ERROR** ,
     + 35HCOULD NOT OBTAIN NAME FOR RESJ FILE )
            CALL ERROR
            return
          endif
      else if  ( MAINUM .EQ. 3) then                    !!! mcp3
C        resjFile = 'TEMP.RESJ'
C        resjFile = '/home/ejm/test_mcp/TEMP.RESJ'
ckwz        call get_user( user,len_user )
ckwz        resjFile = user(1:len_user) // '.TEMP.RESJ'     
        TmpNum=4;
        call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
        resjFile = UserHome(1:HomeDirLength) // '/TEMP.RESJ' // CDATE

c do not move this TEMP file as it is from mcp not ofs  
      endif    

      numTimeSeries = po(2)
      idt = po(3)
      iusec = po(4)

C    CHECK IF TIMESERIES ARE DEFINED
      do  i = 0, (numTimeSeries - 1)

        d_index(i + 1) = to(i + 1) + (ida - idadat) * 24/idt + 
     1                  ihr/idt - 2
                                       !!! TimeSeries Time Interval
                                       !!! The minus two is for two reasons.
                                       !!! RESJ is accessing the array as a 
                                       !!! C array, therefore array subscripting 
                                       !!! starts at 0 and not 1. Second to(i+1)
                                       !!! points to the starting position which 
                                       !!! should have one subtracted from it.
      end do

      



C    CALL C WRAPPER FOR THE RES-J C++ RESJ::execute method
C      write(*,IODBUG) 'ENTERING EXECUTE'
      
      call execute58( resjFile, CO, PO, D, 
     +  d_index,  ICDAY(1), ICHOUR(1), NCSTOR, IDA,
     +  IHR, LDA, LHR,
     +  IFILLC, iusec, ibug, IPR, IODBUG, ierr  )

C      do i = 1, 300  
C         write(IODBUG, *) i, d(i)
C      end do

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** EXIT ex58'
      endif
       

       return
       end
