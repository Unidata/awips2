C MEMBER INA16
C  (from old member MCPIN16)
C
      SUBROUTINE INA16(VA,NA,XM,NLT,TSX,LD,VO)
C.......................................................................
C     THIS SUBROUTINE INITIALIZES THE MULTIYEAR CARRYOVER VALUES
C     FOR THE MCP STATISTICS OPERATION
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C        LARRY BRAZIL - HRL   APRIL 1980   VERSION 1
C.......................................................................
      DIMENSION VA(1),NA(1),XM(1),NLT(1),TSX(1),LD(1),VO(1)
C
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/ina16.f,v $
     . $',                                                             '
     .$Id: ina16.f,v 1.2 1996/07/11 19:29:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** INA16 ENTERED)
C.......................................................................
C     INITIALIZE ARRAYS TO ZERO.
      DO 100 I=1,24
  100 VA(I)=0.0
      DO 110 I=1,36
  110 NA(I)=0
      DO 120 I=1,158
  120 XM(I)=0.0
      DO 130 I=1,7
  130 NLT(I)=0
      DO 140 I=1,31
  140 TSX(I)=0.0
      DO 150 I=1,75
  150 LD(I)=0
      DO 160 I=1,50
  160 VO(I)=0.0
C
      RETURN
      END
