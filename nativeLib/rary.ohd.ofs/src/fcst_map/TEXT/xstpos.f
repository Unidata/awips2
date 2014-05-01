C MEMBER XSTPOS
C  (from old member PPXSTPOS)
C
      SUBROUTINE XSTPOS(PP24,PT24,PPVR,PTVR,KHR)
C.......................................
C     THIS SUBROUTINE SETS ALL ESTIMATED VALUES STORED AS
C        NEGATIVES BACK TO A POSITIVE VALUE.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- FEBRUARY 1983
C.......................................
      INTEGER*2 PP24(1),PT24(1),PPVR(1),PTVR(1)
      INTEGER*2 MSNG24,MSNG6,MSGMDR,MSNGSR
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1 MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xstpos.f,v $
     . $',                                                             '
     .$Id: xstpos.f,v 1.1 1995/09/17 19:00:14 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.1) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** XSTPOS ENTERED)
C.......................................
C     INITIAL VALUE
      N6=KHR/6
C.......................................
C     BEGIN LOOP THROUGH ALL STATIONS
      DO 100 N=1,NSLT24
      I=(N-1)*5+1
      IF(PT24(I).EQ.0) GO TO 100
C
C     CHECK 24 HOUR VALUE FOR NEGATIVE.
      IF(PP24(N).GE.0) GO TO 105
      IF(PP24(N).EQ.MSNG24) GO TO 105
      IF(PP24(N).EQ.-9998) GO TO 101
      PP24(N)=-PP24(N)
      GO TO 105
  101 PP24(N)=0
C
C     CHECK IF STATION HAS 6 HOUR DATA
  105 I6=PT24(I+2)
      IF(I6.LE.0) GO TO 100
C
C     STATION HAS 6 HOUR DATA -- CHECK FOR NEGATIVES
      IF (PTVR(I6).LT.0) PTVR(I6)=-PTVR(I6)
      L6=PTVR(I6+3)
      IF((PPVR(L6).EQ.MSNG6).OR.(PPVR(L6).GE.0)) GO TO 100
      L=L6-1
      DO 110 K=1,N6
  110 PPVR(L+K)=PPVR(L+K)+1100
  100 CONTINUE
C     ALL VALUES SET TO POSITIVE
C.......................................
      IF(IPTRCE.GE.1) WRITE(IOPDBG,901)
  901 FORMAT(1H0,14H** EXIT XSTPOS)
      RETURN
      END
