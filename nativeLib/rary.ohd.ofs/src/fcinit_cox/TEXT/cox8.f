C MEMBER COX8
C  (from old member FCCOX8)
C.......................................................................
      SUBROUTINE COX8(POLD,COLD,PONEW,CONEW)
C
C.......................................................................
C     SUBROUTINE ADJUSTS AND TRANSFERS OLD CARRYOVER VALUES,WHENEVER
C     POSSIBLE, FOR CHANGES MADE TO PARAMETER VALUES.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE  APRIL 1981
C.......................................................................
      INTEGER PEOLD,PENEW
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),C8(2)
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox8.f,v $
     . $',                                                             '
     .$Id: cox8.f,v 1.1 1995/09/17 18:47:30 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA C8/4HCOX8,4H    /
C.......................................................................
C     CHECK TRACE LEVEL
      CALL FPRBUG(C8,1,8,IBUG)
C.......................................................................
C     IF PONEW DOES NOT SPECIFY A PE TIME SERIES, NO CARRYOVER.
C     IF PONEW DOES SPECIFY A PE TIME SERIES:
C         1)LEAVE CARRYOVER AS IS IF POLD DOES NOT SPECIFY PE DATA
C         2)SET CONEW=COLD IF POLD DOES SPECIFY PE DATA
C
      WSOLD=POLD(11)
      WSNEW=PONEW(11)
      IF(WSNEW.LE.0.)GO TO 20
      PENEW=PONEW(12)
      IF(PENEW.EQ.0)GO TO 20
      IF(WSOLD.LE.0.)GO TO 20
      PEOLD=POLD(12)
      IF(PEOLD.EQ.0)GO TO 20
      CONEW(1)=COLD(1)
 20   CONTINUE
      RETURN
      END
