C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (PRETTYPRINT58)
C prp58.f
C
C Print Parameter routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C 06/29/01	James R. VanShaar, RTi	fixed resj name for
C					mcp3
C PO    I       a real array, internally dimensioned PO(1)
C               contains nothing for RES-J
C
C 
       subroutine prp58( po )
C
C      implicit none
C
C   COMMON BLOCKS
C
C    PROGRAM IN USE
      INCLUDE 'common/fprog'
C      
CC    INTEGER MAINUM
CC    REAL    VERS 
CC    REAL    VDATE(2)
CC    REAL    PNAME(5)
CC    INTEGER NDD
C  
C    FORTRAN UNIT NUMBERS
      INCLUDE 'common/ionum'
C
CC    INTEGER IN                ! Unit number of standard input device
CC    INTEGER IPR               ! Unit number of standard print file
CC    INTEGER IPU               ! Unit number of punch device
C
C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
CC    INTEGER ISEG(2)           ! Identifier for segment
CC    INTEGER IOPNUM            ! Operation number currently executed
C
      CHARACTER*8  oname        ! 8 char name of operation being executed

C
C    DEBUG INFO
      INCLUDE 'common/fdbug'
CC    INTEGER IODBUG            ! Unit number ofr debug printout.
CC    INTEGER ITRACE            ! Trace level.
CC    INTEGER IDBALL            ! debug info requested for all ops?
CC    INTEGER NDEBUG            ! number of ops with debug requested
CC    INTEGER IDEBUG(20)        ! List of ops with debug requested

      INCLUDE 'common/cdate'
C
C    PASSED VARIABLES
      REAL po(*)                ! Real array containing the parameteric data
        

C    LOCAL VARIABLES
      
      INTEGER       ierr        ! 0 if no error
      CHARACTER*128 resjFile
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
ckwz change user to UserHome for the TEMP.RESJ file
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/prp58.f,v $
     . $',                                                             '
     .$Id: prp58.f,v 1.8 2004/10/08 19:23:28 hank Exp $
     . $' /
C    ===================================================================
C

C    DATA
      DATA  oname / 'RES-J   ' /

C    DEBUG INITIALIZATION

      ierr = 0
      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** ENTER prp58'
      endif
      CALL FOPCDE ( oname, iprp58 )
      CALL FPRBUG ( oname, 1, iprp58, ibug )

C    DETERMINE FILENAME
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
ckwz get user home dir. instead of user name
        TmpNum=4
        call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
        resjFile = UserHome(1:HomeDirLength) // '/TEMP.RESJ' // CDATE

c leave this TEMP file in the HOME as it is from mcp  
       endif    

C    CALL C WRAPPER FOR THE RES-J C++ RESJ::prettyprint58 method
      call prettyprint58( resjFile, IODBUG, IPR,  ierr )

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** EXIT prp58'
      endif



       return
       end
