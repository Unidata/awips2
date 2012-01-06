C MEMBER HPFNUM
C  (from old member HCLDELFN)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HPFNUM (IFNUM,ISTAT)
C
C          ROUTINE:  HPFNUM
C
C             VERSION:  1.0.0
C
C                DATE:  2-1-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE PLACES A FUNCTION NUMBER IN THE PORTION OF
C    THE GLOBAL HCL INDEX USED TO SAVE DELETED FUNCTION NUMBERS
C    CHECKS TO MAKE SURE THERE ARE LESS THAN 1000 SAVED NUMBERS
C    AND NOT MORE THAN 500 LOCAL OR GLOBAL NUMBERS
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IFNUM      I     I     1    DELETED FUNCTION NUMBER
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     1=INVALID FUNCTION #
C                                     2=FILE FULL
C                                     3=DAIO OR SYSTEM ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER ICNTL(4),IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hpfnum.f,v $
     . $',                                                             '
     .$Id: hpfnum.f,v 1.1 1995/09/17 18:42:42 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      ISTAT=0
C
C       CHECK FOR VALID FUNCTION NUMBER
C
      IF (IFNUM.GT.0.AND.IFNUM.LE.1000) GO TO 20
      WRITE (LPE,10)
10    FORMAT (' **ERROR**  INVALID FUNCTION NUMBER PASSED TO HPFNUM')
      ISTAT=1
      GO TO 110
20    CONTINUE
C
C       READ THE CONTROLS
C
      IXREC=HINDEX(4,8)
      IXUNIT=KINDXG
      CALL UREADT (IXUNIT,IXREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C       CALCULATE WHERE LAST WORD IS
C
      IREC=ICNTL(2) / 4+IXREC+1
      IPOS=MOD(ICNTL(2),4)+1
C
C       UPDATE POINTERS AND CHECK FOR ROOM
C
      ICNTL(2)=ICNTL(2)+1
      IF (ICNTL(2).LE.ICNTL(1)) GO TO 40
      WRITE (LPE,30)
30    FORMAT (' **ERROR** INDEX IS FULL OF DELETED FUNCTION NUMBERS')
      ISTAT=2
      GO TO 110
40    CONTINUE
      IF (IFNUM.GE.500) GO TO 60
      ICNTL(3)=ICNTL(3)+1
      IF (ICNTL(3).LE.500) GO TO 80
      WRITE (LPE,50)
50    FORMAT (' **ERROR** MAXIMUM NUMBER OF DELETED GLOBAL FUNCTIONS')
      ISTAT=2
      GO TO 110
60    CONTINUE
      ICNTL(4)=ICNTL(4)+1
      IF (ICNTL(4).LE.500) GO TO 80
      WRITE (LPE,70)
70    FORMAT (' **ERROR** MAXIMUM NUMBER OF DELETED LOCAL FUNCTIONS')
      ISTAT=2
      GO TO 110
80    CONTINUE
C
C        READ AND WRITE THE RECORDS
C
      CALL UREADT (IXUNIT,IREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      IXBUF(IPOS)=IFNUM
      CALL UWRITT(IXUNIT,IREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      CALL UWRITT(IXUNIT,IXREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      GO TO 110
90    CONTINUE
C
C       DAIO ERROR
C
      WRITE (LPE,100)
100   FORMAT (' **ERROR**  DAIO OR SYSTEM ERROR IN HPFNUM')
      ISTAT=3
110   CONTINUE
C
C       WRITE DEBUG AND RETURN
C
      IF (IHCLDB.EQ.3) WRITE (IOGDB,120) IFNUM,IREC,IPOS,IXBUF,ICNTL
120   FORMAT (' HPFNUM EXECUTED FOR FUNCTION # ',I3,
     1    'WROTE TO RECORD #',I6,' IN POSITION',I3/' INDEX RECORD=',
     2    4I5/' CONTROL RECORD=',4I5)
C
      RETURN
C
      END
