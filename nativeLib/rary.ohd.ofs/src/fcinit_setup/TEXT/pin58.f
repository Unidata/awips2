C$PRAGMA C (get_apps_defaults)
cc AV added for pgf90 port 7/3/01
C$PRAGMA C (INPUT_PARAMETERS58)
C pin58.f
C
C Execution routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C 03/04/01	James R. VanShaar, RTi
C                       Adjusted to preserve temporary status of operation
C                       definition (po(1)=0) for appropriate construcion 
C                       of resj filenames in forthcoming prp58 and prc58
C 05/09/01	JRV, RTi	Removed call to ERROR returning from
C				input_parameters58 as the call to ERROR is now
C				handled internally to input_parameters58, etc.
C
C PO    O       a real array, internally dimensioned PO(1)
C
C PO(1)         contains the string TEMP, or DEFN
C               depending on whether the segment has been 
C               defined yet.
C
C PO(2)                 computational timestep
C PO(3)                 number of time series
C PO(4)                 iusec
C PO(5 + 5 * I)         data identifier of time series I
C PO(6 + 5 * I)         data identifier of time series I
C PO(7 + 5 * I)         data type of time series I
C PO(8 + 5 * I)         data time interval of time series I
C PO(9 + 5 * I)         IN or OU - specifies if time series is input or output
C       
C       
C
C LEFTP I       integer; indicates how much space is left
C               in the Forecast Component P array
C
C IUSEP O       an integer variable which indicates how much
C               space is used by this operation in the P array
C
C CO    O       a real array, internally dimensioned CO(1)
C               contains carryover values for RES-J
C               operation 
C
C IUSEC O       an integer variable which indicates how much
C               space is used by this operation in the C array
      
      subroutine pin58( po, leftp, iusep, co, leftc, iusec )

      EXTERNAL   JNUM

C       implicit none
C
C   COMMON BLOCKS
C
C    MAIN PROGRAM TYPE
      INCLUDE 'common/fprog'
CC    INTEGER   MAINUM          ! Main program code
CC                              ! 1 = ofs, 2 = esp, 3 = mcp3, 4 = opt3
CC    REAL      VERS            ! Version number
CC    REAL      VDATE(2)        ! Version date
CC    REAL      PNAME(5)        ! program name
CC    INTEGER   NDD             ! Number of days per time series in D array
                                
C    RFS IO UNIT NUMBERS
      INCLUDE 'common/ionum'
CC    INTEGER IN                ! Unit number of standard input device
CC    INTEGER IPR               ! Unit number of standard print file
CC    INTEGER IPU               ! Unit number of punch device

C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
CC    INTEGER ISEG(2)           ! Identifier for segment
CC    INTEGER IOPNUM            ! Operation number currently executed
CC    INTEGER OPNAME(2)         ! 8 character name of operation currently being executed

C    RFS DEBUG INFO
      INCLUDE 'common/fdbug'
CC    INTEGER IODBUG            ! Unit number ofr debug printout 
CC    INTEGER ITRACE            ! Trace level 
CC    INTEGER IDBALL            ! debug info requested for all ops? 
CC    INTEGER NDEBUG            ! number of ops with debug requested
CC    INTEGER IDEBUG(20)        ! List of ops with debug requested              

      INCLUDE 'common/cdate'

C    PASSED VARIABLES
      REAL      po(*)           ! Real array containing parametric data
      REAL      co(*)           ! Real array containing carryover data
      INTEGER   leftp           ! Integer indicating how much space is left in P array
      INTEGER   iusep           ! Integer indicating how much P array space RES-J uses
      INTEGER   leftc           ! Integer indicating how much space is left in C array
      INTEGER   iusec           ! Integer indicating how mucc C array space RES-J uses

C    LOCAL VARIABLES
      
      CHARACTER*300     inLine 
      CHARACTER*128     resjFile
CC    CHARACTER*128     mybin   ! character string to hold name of mybin dirctories 
ckwz      CHARACTER*128     user    ! character string to hold name of user
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
ckwz change user to UserHome for the TEMP.RESJ file
      CHARACTER*8       oname
      INTEGER           unitNum 
      INTEGER           ierr 
      INTEGER           ibug 
      INTEGER           ipin58
      INTEGER           i_0                     !!! 0
      INTEGER           i_1                     !!! 1
      INTEGER           i                       !!! counter
      INTEGER           i_ti                    !!! Time Interval of time series
      INTEGER           i_numval                !!! Number of values per time step in TS
      INTEGER           i_miss                  !!! If Missing Values are allowed
      INTEGER           numTimeSeries           !!! Number of Time Series
      REAL              r_0
      REAL              r_id(2)                 !!! Time Series id
      REAL              r_dt                    !!! Time Series data type
      REAL              r_dc                    !!! Time Series 
      CHARACTER         ch_po(4)              !!! get
      REAL              r_po
      EQUIVALENCE       (ch_po(1), r_po)        !!! used to get time series info out of po array
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/pin58.f,v $
     . $',                                                             '
     .$Id: pin58.f,v 1.11 2004/10/08 19:23:51 hank Exp $
     . $' /
C    ===================================================================
C
C

C    DATA
      DATA  oname / 'RES-J   ' /

      numTimeSeries = 0

C    DEBUG INITIALIZATION

      if (ITRACE.GT.0) then 
        write(IODBUG, *) '*** ENTER pin58'
      endif
      CALL FOPCDE ( oname, ipin58 )
      CALL FPRBUG ( oname, 1, ipin58, ibug ) 


C    P ARRAY
C       P(1) = Integer 0 or 1. Based on this information
C       files opening resj will know whether or not the res-j 
C       information is contained in a temporary file, or in the 
C       permanent fs5file.
C       0 - implies Temp File
C       1 - implies permanent fs5file

      po(1) = 0

C    GET THE NAME OF THE RES-J TEMP FILE
      if ( MAINUM .EQ. 1 ) then                 !!! ofs
        call generateRESJFileName( po, resjFile, iseg, ierr )
          if ( ierr .GT. 0 ) then
            write ( ipr, 101 )
101         FORMAT ( 1H0, 10X, 10H**ERROR** ,
     + 35HCOULD NOT OBTAIN NAME FOR RESJ FILE )
            CALL ERROR
            return
          endif
      else if  ( MAINUM .EQ. 3) then                    !!! mcp3
ckwz        call get_user( user,len_user )
ckwz        resjFile = user(1:len_user) // '.TEMP.RESJ'     
        TmpNum=4
        call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
        resjFile = UserHome(1:HomeDirLength) // '/TEMP.RESJ' // CDATE

c leave this TEMP file alone as it is from mcp      
       endif    
      
C    OPEN A TEMP FILE FOR RES-J
C
C ---- /nonawips/usr/apps/nwsrfs/sys_files/FILEUNIT
C Add the following line in file /fs/awips/rfc/nwsrfs/sys_files/FILEUNIT
C FCINIT            TEMP.RESJ       99              # Temporary file for RES-J 
C
      call opfile( resjFile, 'TEMP.RESJ ', 'SEQUENTIAL ', 'UNKNOWN ', 
     +  'FORMATTED ', 0, unitNum, ierr )

C The following line was commented out to preserve the usage of the ..../TEMP.*
C filename in forthcoming prp58 and prc58.  The second line (po(1) = 0) is
C somewhat redundant in that po(1) is assigned to 0 above.
C The implications of this change are that in the output file for fcinit
C the 'ResJStatus -> Using "/fs..../" for RESJ operation' output will show
C the temporary filename, which upon successful (re)definition of the segment
C will no longer exist; it will be renamed without the 'TEMP.' portion.
C fputsg (called by fsegdef) will reassign the value of po(1) to one prior
C to usage in cox58 and storage in the database.
C Calls to rmRESJfs5files and mvRESJFilesToPerm in fupdco handle the renaming
C of the resj files to remove the 'TEMP.' portion of the filename.
C      po(1) = 1
      po(1) = 0

      if ( ierr .GT. 0 ) then
        write ( ipr, 102 ) 
102     FORMAT ( 1H0, 10X, 10H**ERROR** , 
     + 29HCOULD NOT OPEN TEMP.RESJ FILE )
        CALL ERROR
        po(1) = 0
        return
      endif

C    READ ONE CARD AT A TIME, AND WRITE TO RES-J TEMP FILE
C    READ CARDS UNTIL "ENDRES-J" IS ENCOUNTERED


CCC      read ( in, 100 ) inLine 
      read ( in, 100 ) inLine 
      do while (inLine(1:8) .NE. 'ENDRES-J')
        write( unitNum, 100 ) inLine
        read ( in, 100 ) inLine 
      end do

C    CLOSE RES-J TEMP FILE
      call clfile( 'TEMP.RESJ ', unitNum, ierr )

C    CALL C ROUTINE WHICH WRAPS AROUND C++ ResJ::input_parameters method
C resjFile - input -    name of resj fs5file for reading 
C po     - output -     parameter array containing TS informations
C co     - output -     carryover array containing all carryover values
C iusec  - output -     number of CO array elements used
C ipr    - input -      unit number for error messages
C iodebug  - input -    unit number for debug messages
C ibug   - input -      if debug info should be output, this number is greater
C                         than 0
C ierr   - output -     0 if no error, 1 otherwise`
      call input_parameters58( resjFile, po(1), co, iusec, ipr, IODBUG, 
     + ibug, ierr )

C    CHECK FOR ERRORS RETURNING FROM RES-J
      if (ierr.GE.1) then
C        call error
        return
      endif
     
      numTimeSeries = PO(2)
      idt = PO(3)
      PO(4) = iusec

C    CHECK IF TIMESERIES ARE DEFINED
      do  i = 0, (numTimeSeries - 1)
        i_0 = 0
        r_0 = 0.0
        r_id(1) = po(5 + (5 * i))       !!! TimeSeries ID
        r_id(2) = po(6 + (5 * i))       !!! TimeSeries ID
        r_dt = po(7 + (5 * i))          !!! TimeSeries Data Type

        r_po = po(8 + (5 * i))     
        i_ti = JNUM( ch_po(3) ) * 10 + JNUM( ch_po(4) )
                                !!! TimeSeries Time Interval 

C    WRITE OUT TIMESERIES INFO AS WE GO; this may need to be removed at some point
      write(ipr,999) r_id, r_dt, i_ti
999   format( 1H0, 10X, 'TS ID ', 2A4, ' Data Type ', A4, ' IDT ', I4 )

        CALL FDCODE( r_dt, r_foo1, r_foo2, i_miss, i_foo3, 
     + r_foo4, i_foo5, i_foo6 )
                                        !!! Determine if TS allows missing values
        r_dc = 1
        i_numval = 0
        i_1 = 1
        CALL CHEKTS( r_id, r_dt, i_ti, r_0, r_dc, i_1, i_0, ierr )
        IF (ierr.GE.1) THEN
           WRITE( IPR, 909 ) r_id, r_dt, i_ti
  909      FORMAT( 1H0, 10X, 10H**ERROR** ,
     1  'THERE IS AN ERROR WITH THE TIME SERIES  ', 
     2   2A4, ' ', A4, ' ', I4 )
           CALL ERROR
           RETURN
        END IF


      end do
        

C    SET IUSEP
      IUSEP = numTimeSeries * 5 + 4
      CALL CHECKP( IUSEP, LEFTP, IERR )
      IF (IERR.GE.1) THEN
          WRITE( IPR, 902 )
  902     FORMAT( 1H0, 10X, 10H**ERROR** ,
     1 50HTHERE IS NOT ENOUGH SPACE AVAILABLE IN THE P ARRAY )
          CALL ERROR
          RETURN
      END IF


C    SET IUSEC
      CALL CHECKC( IUSEC, LEFTC, IERR )
      IF (IERR.GE.1) THEN
          WRITE( IPR, 903 )
  903     FORMAT( 1H0, 10X, 10H**ERROR** ,
     1 50HTHERE IS NOT ENOUGH SPACE AVAILABLE IN THE C ARRAY )
          CALL ERROR
          RETURN
      END IF


C    OUTPUT DEBUG INFO
      if (ITRACE.GT.0) then 
        write(IODBUG, *) '*** EXIT pin58'
      endif

  100 format ( A80 )
      return
      end
