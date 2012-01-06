C MEMBER FSTCO1
C  (from old member FCEX1)
C
      SUBROUTINE FSTCO1(C,IFRZE)
C.......................................
C     THIS SUBROUTINE TRANSFERS CARRYOVER VARIABLES INTO THE CARRYOVER
C          ARRAY USED IN THE 'SAC-SMA' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ....
C           ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION C(1)
      REAL LZTWC,LZFSC,LZFPC
C
C     COMMON BLOCKS
      COMMON/FSMCO1/UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO(6),RSUM(7),
     1PPE,PSC,PTA,PWE
      COMMON/FSMPT1/IROC,IROCO,ISC,ISCO,IET,IETCO,LWE,NXCO,NXFCO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sac/RCS/fstco1.f,v $
     . $',                                                             '
     .$Id: fstco1.f,v 1.1 1995/09/17 18:58:17 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     SOIL-MOISTURE CARRYOVER.
      C(1)=UZTWC
      C(2)=UZFWC
      C(3)=LZTWC
      C(4)=LZFSC
      C(5)=LZFPC
      C(6)=ADIMC
      IF(NXCO.EQ.0) GO TO 110
      IF(IET.GT.0) C(IETCO+6)=PPE
      IF(ISC.GT.0) C(ISCO+6)=PSC
      IF(IROC.EQ.0) GO TO 110
      J=IROCO+6-1
      DO 101 I=1,7
  101 C(J+I)=RSUM(I)
C.......................................
C     FROZEN GROUND CARRYOVER.
  110 IF(IFRZE.EQ.0) RETURN
      DO 111 I=1,6
      J=NXCO+6+I
  111 C(J)=FGCO(I)
      C(NXCO+13)=PTA
      IF(LWE.GT.0) C(NXCO+14)=PWE
C.......................................
      RETURN
      END
