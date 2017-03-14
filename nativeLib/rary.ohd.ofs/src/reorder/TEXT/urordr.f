C MEMBER URORDR
C-----------------------------------------------------------------------
C
       SUBROUTINE URORDR (LARRAY,ARRAY,ISTAT)
C
C          SUBROUTINE:  URORDR
C
C   7/8/86  TASK 282 UPDATES, PASS WORK ARRAY
C  2/19/85 SRS MOVE UNITS FROM PRD FILE CONTROLS FROM NEW TO OLD FOR
C              SORDER SO RPRDH WILL READ FROM CORRECT FILES.
C             VERSION:  1.0.0
C
C                DATE:  7-29-85
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE CALLS THE PPINIT PROGRAM @ORDER COMMAND ROUTINE TO
C    WRITE THE COMPUTATIONAL ORDER PARAMETER ARRAYS TO THE NEW
C    PREPROCESSOR PARAMETRIC DATA BASE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C     LARRAY      I       I     1     LENGTH OF ARRAY
C      ARRAY      R      IO   LARRAY   WORK ARRAY
C       ISTAT      I     O     1     STATUS CODE
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
       REAL ARRAY(LARRAY)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urordr.f,v $
     . $',                                                             '
     .$Id: urordr.f,v 1.1 1995/09/17 19:17:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPTR.GT.0) WRITE (IOGDB,70)
C
      ISTAT=0
      NFLD=-1
C
C  DETERMINE COMPUTATIONL ORDER
      IAMORD=1
      IPRINT=0
      CALL SORDER (LARRAY,ARRAY,NFLD,IPRINT,IERR)
      IAMORD=0
      IF (IERR.EQ.0) CALL SULINE (LP,2)
      IF (IERR.EQ.0) WRITE (LP,90)
      IF (IERR.GT.0) WRITE (LP,110)
      IF (IERR.GT.0) CALL SUWRNS (LP,2,-1)
      IF (IERR.GT.0) IWURFL=1
C
      IF (IPPTR.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPTR.GT.0) WRITE (IOGDB,120)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' *** ENTER URORDR')
90    FORMAT ('0*** NOTE - COMPUTATIONAL ORDER PARAMETERS ',
     *   'WERE SUCCESSFULLY WRITTEN.')
110   FORMAT ('0*** WARNING - IN URORDR - ERRORS ENCOUNTERED WRITING ',
     *   'COMPUTATIONAL ORDER PARAMETERS TO NEW FILES.')
120   FORMAT (' *** EXIT URORDR')
C
      END
