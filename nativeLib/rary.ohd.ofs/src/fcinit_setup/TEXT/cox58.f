C$PRAGMA C (CARRYOVERTRANSFER58)
C cox58.f 
C
C Carryover Transfer routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C 03/03/01	James R. VanShaar, RTi	Reinstated the
C					carryovertransfer58
C					routine
C POLD  I       a real array, internally dimensioned POLD(1)
C               old PO array for RES-J operation
C COLD  I       a real array
C               old CO array for RES-J operation
C PONEW I       a real array new PO array 
C CONEW I,O     a real array, contains the new CO for the 
C               operation 
C
       subroutine COX58( POLD, COLD, PONEW, CONEW )
C      implicit none
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
CC    INTEGER OPNAME(2)         ! 8 char name of operatn currently executing

C    PASSED VARIABLES
       REAL pold(*)             ! Real array containing the previous RES-J operation name(s)
       REAL cold(*)             ! Real array containing the old RES-J carryover
       REAL ponew(*)            ! Real array containing the new RES-J operation names(s)
       REAL conew(*)            ! Real array containg the new RES-J carryover

C    LOCAL VARIABLES 
      CHARACTER*128     resjFOLD        ! name of res-j fs5file
      CHARACTER*128     resjFNEW        ! name of res-j fs5file
CC    CHARACTER*80      inLine          ! character buffer to hold a line of data
CC    INTEGER           unitNum         ! unitNum for opening
      CHARACTER*8       oname   ! Operation ID
      INTEGER ibug              ! Flag - 0 if DEBUG off, 1 otherwise
      INTEGER icox58            ! Contains 58 for operation number
      INTEGER ierr              ! 0 - no error, 1 - error
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/cox58.f,v $
     . $',                                                             '
     .$Id: cox58.f,v 1.6 2002/02/11 13:55:24 michaelo Exp $
     . $' /
C    ===================================================================
C


C    DATA
      DATA  oname / 'RES-J   ' /

C    DEBUG INITIALIZATION

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** ENTER cox58'
      endif
      CALL FOPCDE ( oname, icox58 )
      CALL FPRBUG ( oname, 1, icox58, ibug )
      ierr = 0


C Get resj File name for old definition
      call generateRESJFileName( POLD, resjFOLD, iseg, ierr )

      if ( ierr .GT. 0 ) then
        write ( ipr, 101 )
101     FORMAT ( 1H0, 10X, 10H**ERROR** ,
     + 35HCOULD NOT OBTAIN NAME FOR RESJ FILE )
        CALL ERROR
        return
      endif

C Get resj File name for old definition
C PONEW is set as a defined segment (as opposed to temporary) by fputsg in
C fsegdef which will cause the generateRESJFileName to return the wrong resj
C file.  To cause the function to return the proper name (TEMP.*) we will 
C temporarily change the value of the PONEW array characterizing it as defined.
      PONEW(1) = 0;
      call generateRESJFileName( PONEW, resjFNEW, iseg, ierr )
      PONEW(1) = 1;

C    CALL C WRAPPER FOR THE RES-J C++ RESJ::carryover_transfer method
C    Re-instated: JRV 3/3/01
C      ierr = input_parameters( resjFile, 
      call carryovertransfer58( COLD, CONEW, POLD, PONEW, 
     + resjFOLD, resjFNEW, ibug, ipr, iodbug, ierr ) 

C
C................................................
C EJM RTi Modification 12/01/97
C   rm all old RESJ fs5files and mv temp RES-J
C   fs5files to permanent RES-J fs5files
C   works for both resegdefs and segdefs
C
ccc       ier = rmRESJfs5files( POLD, iseg  )   ! remove the old fs5files for the segment
ccc       ier = mvRESJFilesToPerm( PO, iseg )    ! move the temporary RESJ files to
                                                ! the permanent fs5files
C END EJM MODIFICATIONS

c    OUTPUT DEBUG INFO
      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** EXIT cox58'
      endif

       return
       end
