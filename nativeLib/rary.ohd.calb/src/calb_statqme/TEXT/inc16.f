C MEMBER INC16
C  (from old member MCPIN16)
C
      SUBROUTINE INC16(I1)
C.......................................................................
C     THIS SUBROUTINE INITIALIZES ARRAYS FOR CUMMULATIVE FREQ. OPTION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
      DIMENSION I1(1)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/inc16.f,v $
     . $',                                                             '
     .$Id: inc16.f,v 1.2 1996/07/11 19:29:54 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,902)
  902 FORMAT(1H0,16H** INC16 ENTERED)
C
      DO 300 I=1,51
  300 I1(I)=0
C
      RETURN
      END
