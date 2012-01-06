C MEMBER INB16
C  (from old member MCPIN16)
C
      SUBROUTINE INB16(R1,IXB,KK)
C.......................................................................
C     THIS SUBROUTINE INITIALIZES ARRAYS FOR QUARTERLY ACCUM. OPTION
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
      DIMENSION R1(1)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/inb16.f,v $
     . $',                                                             '
     .$Id: inb16.f,v 1.2 1996/07/11 19:29:34 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,901)
  901 FORMAT(1H0,16H** INB16 ENTERED)
C
      DO 200 I=1,IXB
  200 R1(I)=0.0
      KK=1
C
      RETURN
      END
