C$PRAGMA C (PUNCH58)
C puc58.f
C
C Card Punch routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C PO    I       a real array, internally dimensioned PO(1)
C                       contains nothing for RES-J
C
C CO    I       a real array, internally dimensioned CO(1)
C               contains carryover values for RES-J
C               operation 
C
C
      subroutine puc58( po, co )

C
C   COMMON BLOCKS
C
C    FORTRAN UNIT NUMBERS
      INCLUDE 'common/ionum'
C
CC    INTEGER IN                ! Unit number of standard input device
CC    INTEGER IPR               ! Unit number of standard print file
CC    INTEGER IPU               ! Unit number of punch device
C
C    DEBUG INFO
      INCLUDE 'common/fdbug'
C
CC    INTEGER IODBUG            ! Unit number ofr debug printout.
CC    INTEGER ITRACE            ! Trace level.
CC    INTEGER IDBALL            ! debug info requested for all ops?
CC    INTEGER NDEBUG            ! number of ops with debug requested
CC    INTEGER IDEBUG(20)        ! List of ops with debug requested

C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
CC    INTEGER ISEG(2)           ! Identifier for segment
CC    INTEGER IOPNUM            ! Operation number currently executed
CC    INTEGER OPNAME(2)         ! 8 character name of operation currently being executed

C
C    PASSED VARIABLES
      dimension po(*), co(1)

C    LOCAL VARIABLES
      CHARACTER*8       oname
      CHARACTER*128     resjFile        ! name of res-j fs5file
CC    CHARACTER*80      inLine          ! character buffer to hold a line of data
CC    INTEGER           unitNum         ! unitNum for opening
      INTEGER           ierr            ! no error = 0, else error = 1
CC    INTEGER           iiostat         ! return status from fortran read statement
      INTEGER           ipuc58          ! contains number 58
      INTEGER           ibug            ! not 0 if debug is one
CC    INTEGER           i               ! counter
CC    INTEGER           j               ! counter
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/puc58.f,v $
     . $',                                                             '
     .$Id: puc58.f,v 1.3 2002/02/11 13:55:39 michaelo Exp $
     . $' /
C    ===================================================================
C

C    DATA
      DATA  oname / 'RES-J   ' /

C    DEBUG INITIALIZATION

      if (ITRACE.GT.0) then 
        write(IODBUG, *) '*** ENTER puc58'
      endif
      CALL FOPCDE ( oname, ipuc58 )
      CALL FPRBUG ( oname, 1, ipuc58, ibug ) 


      call generateRESJFileName( po, resjFile, iseg, ierr )
      if ( ierr .GT. 0 ) then
        write ( ipr, 101 )
101     FORMAT ( 1H0, 10X, 10H**ERROR** ,
     + 35HCOULD NOT OBTAIN NAME FOR RESJ FILE )
        CALL ERROR
        return
      endif
     

      iusec = po(4)

C    CALL PUNCH C++ ROUTINE

      call punch58( resjFile, co, iusec, IPU, IPR, IODBUG, ierr )

C    OUTPUT DEBUG INFO
      if (ITRACE.GT.0) then 
        write(IODBUG, *) '*** EXIT puc58'
      endif


      return
      end
