C MEMBER IND16
C  (from old member MCPIN16)
C
      SUBROUTINE IND16(QEX,SIM,OBS,EXCED)
C.......................................................................
C     THIS SUBROUTINE INITIALIZES ARRAYS FOR Q-EXCEEDENCE PLOT
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
      DIMENSION SIM(1),OBS(1),QEX(1)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/ind16.f,v $
     . $',                                                             '
     .$Id: ind16.f,v 1.2 1996/07/11 19:30:11 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,903)
  903 FORMAT(1H0,16H** IND16 ENTERED)
C
      DO 400 I=1,20
      SIM(I)=0.0
  400 OBS(I)=0.0
C     DETERMINE Q INTERVALS FOR PLOT
      DIV=EXCED/20.0
      DO 410 I=1,20
  410 QEX(I)=I*DIV
C
      RETURN
      END
