C MEMBER COX20
C  (from old member FCCOX20)
C
      SUBROUTINE COX20(POLD,COLD,PONEW,CONEW)
C
C ......................................................................
C
C       THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE CHANGE TIME
C     INTERVAL OPERATION.  OLD CARRYOVER IS USED AS IS IF:
C         1. BOTH PARAMETER SETS NEED CARRYOVER
C     AND 2. THE TWO TIME SERIES ARE A.) INST OR B.) MEAN TO INST, OR
C            C.) MEAN OR ACCM AND OLD AND NEW TIME INTERVALS ARE EQUAL.
C ONLY DO XFER IF CASES ARE THE SAME.
C
C ......................................................................
C
C     SUBROUTINE ORIGINALLY WRITTEN BY :
C        ED VANBLARGAN - HRL   MAY, 1981
C
C ......................................................................
C
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
      DIMENSION ISUBN(2),ACASE(2),AINT(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox20.f,v $
     . $',                                                             '
     .$Id: cox20.f,v 1.1 1995/09/17 18:47:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ISUBN/4HCOX2,4H0   /,
     $ ACASE,AINT/4H    ,4HCASE,4HTIME,4H INT/
C
C     UTILITY SUBROUTINE TO GET TRACE LEVEL AND DEBUG.
C
      CALL FPRBUG(ISUBN,1,20,IBUG)
C
C GET ICASE AND INITIALIZE CONEW
C     GET OLD AND NEW OUTPUT TIME INTERVALS
C
      ICASE=POLD(12)
      ICSNEW=PONEW(12)
      CONEW(1)=0.0
      IF (ICSNEW.EQ.7) CONEW(2)=0.0
      ITBOLD=POLD(9)
      ITBNEW=PONEW(9)
C
C     CHECK IF BOTH OLD AND NEW HAVE CO VALUES.
C
      NEEDC=PONEW(10)
      IF (NEEDC.EQ.0) GO TO 999
      NEEDC=POLD(10)
      IF (NEEDC.EQ.0) GO TO 999
C
C CHECK IF CASE HAS CHANGED
      IF (ICASE.EQ.ICSNEW) GO TO 50
      WRITE(IPR,100) ACASE
      GO TO 999
50    IF (ICASE.GE.5) GO TO 200
C
C     CHECK IF OLD AND NEW TIME INTERVALS ARE EQUAL
C     FOR MEAN OR ACCM. IF NOT, USE DEFAULT CO=0.
C
      IF (ITBOLD.EQ.ITBNEW) GO TO 200
      WRITE (IPR,100) AINT
100   FORMAT(1H0,10X,35H**WARNING** OLD CARRYOVER CANNOT BE,
     *32H USED. DEFAULT CO OF ZERO USED. ,2A4,9H CHANGED.)
      GO TO 999
C
C     OLD CARRYOVER CAN BE USED
C
200   CONEW(1)=COLD(1)
      IF (ICASE.EQ.7) CONEW(2)=COLD(2)
999   RETURN
      END
