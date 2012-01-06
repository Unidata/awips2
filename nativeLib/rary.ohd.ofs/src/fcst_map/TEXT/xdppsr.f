C MEMBER XDPPSR
C  (from old member PPXDPPSR)
C
      SUBROUTINE XDPPSR(NSSR,PPSR,MSNGSR,LHRCDE,KHR)
C.......................................
C     THIS SUBROUTINE DECODES STRANGER REPORT PRECIPITATION VALUES
C     READ FROM THE PPDB.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- SEPTEMBER 1984
C.......................................
      INTEGER*2 PPSR(1),MSNGSR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xdppsr.f,v $
     . $',                                                             '
     .$Id: xdppsr.f,v 1.1 1995/09/17 18:59:32 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     DECODE PRECIPITATION VALUE.
      DO 100 J=1,NSSR
      I=J*3
      IF (PPSR(I).EQ.MSNGSR) GO TO 101
      ICODE=PPSR(I)-((PPSR(I)/10)*10)
      ICODE=IABS(ICODE)
      IF ((KHR.EQ.24).AND.(ICODE.GT.0)) GO TO 101
      IF (ICODE.LT.LHRCDE) GO TO 101
      PPSR(I)=(PPSR(I)/10)+3000
      GO TO 100
  101 PPSR(I)=-9999
  100 CONTINUE
      MSNGSR=-9999
C.......................................
      RETURN
      END
