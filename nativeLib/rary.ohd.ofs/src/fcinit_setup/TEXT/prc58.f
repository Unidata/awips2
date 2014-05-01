C$PRAGMA C (get_apps_defaults)
C prc58.f
C
C Print Carryover routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C PO    I       a real array, internally dimensioned PO(1)
C               contains nothing for RES-J
C
C CO    I       a real array, internally dimensioned CO(1)
C               contains carryover values for RES-J
C               operation 
C
C 
       subroutine prc58( po, co )

C      implicit none
C
C   COMMON BLOCKS
C
C       PROGRAM IN USE
      INCLUDE 'common/fprog'
C       
CC    INTEGER MAINUM
CC    REAL    VERS 
CC    REAL    VDATE(2)
CC    REAL    PNAME(5)
CC    INTEGER NDD
C
CC    CHARACTER*20 componentName
CC    CHARACTER*20 componentID
CC    CHARACTER*20 methodName
CC    CHARACTER*20 methodID
CC    CHARACTER*20 carryoverDescriptor
      CHARACTER*8  oname
      
C
C    FORTRAN UNIT NUMBERS
      INCLUDE 'common/ionum'
CC    INTEGER IN      ! Unit number of standard input device
CC    INTEGER IPR     ! Unit number of standard print file
CC    INTEGER IPU     ! Unit number of punch device

C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
CC    INTEGER ISEG(2)           ! Identifier for segment
CC    INTEGER IOPNUM            ! Operation number currently executed
CC    INTEGER OPNAME(2)         ! 8 character name of operation currently being executed


C    RFS DEBUG INFO
      INCLUDE 'common/fdbug'
CC    INTEGER IODBUG            ! Unit number ofr debug printout.
CC    INTEGER ITRACE            ! Trace level.
CC    INTEGER IDBALL
CC    INTEGER NDEBUG            ! number of ops with debug requested
CC    INTEGER IDEBUG(20)        ! List of ops with debug requested

      INCLUDE 'common/cdate'

C    PASSED VARIABLES
      REAL po(*)
      REAL co(*)

C    LOCAL VARIABLES
      
      CHARACTER*128 resjFile    ! filename of input file
      CHARACTER*128 UserHome	! character string to hold user home dir
      INTEGER HomeDirLength,TmpNum
ckwz change user to UserHome for the TEMP.RESJ file
CC    INTEGER       nextCO      ! contains location of next component in CO array, -999 if last
CC    INTEGER       lastCO  ! contains location of next component in CO array
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/prc58.f,v $
     . $',                                                             '
     .$Id: prc58.f,v 1.6 2004/10/08 19:23:40 hank Exp $
     . $' /
C    ===================================================================
C
C    DATA
      DATA  oname / 'RES-J   ' /

C    DEBUG INITIALIZATION

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** ENTER prc58'
      endif
      CALL FOPCDE ( oname, iprc58 )
      CALL FPRBUG ( oname, 1, iprc58, ibug )
      
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
        TmpNum=4
        call get_apps_defaults("HOME", TmpNum, UserHome, HomeDirLength)
        resjFile = UserHome(1:HomeDirLength) // '/TEMP.RESJ' // CDATE

c leave this temp file in the user home as it is for mcp 
       endif    


C    CALL C WRAPPER FOR THE RES-J C++ RESJ::print_carryover method
C
C EJM Comment out until print_carryover has been written
C      ierr = print_carryover58( PO, CO, resjFile, ibug, ipr, iodbug ) 



      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** EXIT prc58'
      endif

      return
      end
