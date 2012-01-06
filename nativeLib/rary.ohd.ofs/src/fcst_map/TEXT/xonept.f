C MEMBER XONEPT
C  (from old member PPXONEPT)
C
      SUBROUTINE XONEPT(N,I6,L,PP24,PT24,PPVR,PTVR,MSNG24,MSNG6,
     1N6,IP24,CHAR,IREC,PARM,LWORK,MSG,IERR)
C.......................................
C     THIS SUBROUTINE TRIES TO FILL IN MISSING DATA AT A
C     PCPN STATION FROM ANOTHER STATION AT THE SAME POINT.
C.......................................
C     WRITTEN BY --ERIC ANDERSON, HRL-- APRIL, 1984
C.......................................
      INTEGER*2  PP24(1),PT24(1),PPVR(1),PTVR(1),CHAR(1)
      INTEGER*2 MSNG24,MSNG6,TOT
      DIMENSION PARM(1),STA(2)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xonept.f,v $
     . $',                                                             '
     .$Id: xonept.f,v 1.1 1995/09/17 19:00:02 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA PCPN/4HPCPN/
      DATA BLANK/4H    /
C.......................................
C     CHECK TRACE LEVEL
      IF (IPTRCE.GE.1) WRITE(IOPDBG,900) N
 900  FORMAT (1H0,27H** XONEPT ENTERED--STAN NUM,I5)
C.......................................
C     CHECK IF DEBUG ON.
      IBUG=0
      IF (IPBUG(PCPN).EQ.1) IBUG=1
C.......................................
C     INITIAL VALUES
      IREC=IABS(IREC)
      MSG=1
      IERR=0
C.......................................
C     READ PCPN RECORD
      STA(1)=BLANK
      STA(2)=BLANK
      IREC=-IREC
      CALL RPPREC(STA,PCPN,IREC,LWORK,PARM(1),NFILL,INX,ISTAT)
      IF(ISTAT.EQ.0) GO TO 105
      CALL PSTRDC(ISTAT,PCPN,STA,IREC,LWORK,NFILL)
      WRITE(IPR,901)
 901  FORMAT(1H0,40H**FATAL ERROR** THE ABOVE ERROR IS FATAL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
 105  IF (IBUG.EQ.1) CALL PDUMPA(NFILL,PARM(1),PCPN,STA,1)
C.......................................
C     GET CHARACTERISTICS FOR STATION.
      LC=PARM(23)
      CX=1.0
      IF (LC.GT.0) CX=CHAR(LC)*0.01
C.......................................
C     TRY TO GET 24-HOUR TOTAL FROM ALTERNATE GAGES.
      IF (PP24(N).NE.MSNG24) GO TO 120
C
C     CANNOT DO STATIONS WITH SIGNIFICANCE WEIGHTS.
      IW=PARM(19)
      IF(IW.GT.0) GO TO 120
C
C     FIND COLOCATED STATIONS.
      DO 110 K=1,4
      LW=43+(K-1)*5
      LP=23+(K-1)*5
      DO 110 J=1,5
      IF (PARM(LW+J).LT.9999.0) GO TO 110
      IP=PARM(LP+J)
      LD=((IP-1)/5)+1
      IF (PP24(LD).EQ.MSNG24) GO TO 110
      IF  ((PP24(LD).EQ.0).AND.(PT24(IP+4).LT.0)) GO TO 110
      IF (LC.LE.0) GO TO 106
      I=PT24(IP+1)
      IF (I.GE.10000) I=I-10000
      IF (I.LE.0) GO TO 106
      CE=CHAR(I)*0.01
      GO TO 107
 106  CE=CX
 107  PP24(N)=PP24(LD)*(CX/CE)
      PT24(IP24+1)=PT24(IP24+1)+10000
      IF (I6.LE.0) GO TO 150
      GO TO 125
 110  CONTINUE
 120  IF (I6.LE.0) GO TO 99
C.......................................
C     SIX HOUR STATION.
C     FIND COLOCATED STATIONS.
  125 DO 130 K=1,4
      LW=75+(K-1)*3
      LP=63+(K-1)*3
      DO 130 J=1,3
      IF (PARM(LW+J).LT.9999.0) GO TO 130
      IP=PARM(LP+J)
      LD=PTVR(IP+3)-1
      IF (LC.LE.0) GO TO 132
      I24=PTVR(IP+1)
      I=PT24(I24+1)
      IF (I.GE.10000) I=I-10000
      IF (I.LE.0) GO TO 132
      CE=CHAR(I)*0.01
      GO TO 133
 132  CE=CX
 133  TOT=0
      MSG6=0
      DO 135 I=1,N6
      IF (PPVR(L+I).NE.MSNG6) GO TO 137
      IF (PPVR(LD+I).NE.MSNG6) GO TO 136
      MSG6=1
      GO TO 135
 136  PPVR(L+I)=PPVR(LD+I)*(CX/CE)
 137  TOT=TOT+PPVR(L+I)
  135 CONTINUE
      IF (MSG6.EQ.0) GO TO 140
 130  CONTINUE
      GO TO 99
C.......................................
C     ALL 6-HOUR VALUES FILLED IN.
 140  PTVR(I6)=-PTVR(I6)
      IF (PP24(N).NE.MSNG24) GO TO 150
      PP24(N)=TOT
      PT24(IP24+1)=PT24(IP24+1)+10000
C.......................................
C     NO MISSING VALUES REMAIN.
 150  MSG=0
C.......................................
  99  IF (IPTRCE.GE.1) WRITE(IOPDBG,902)
 902  FORMAT(1H0,14H** EXIT XONEPT)
      RETURN
      END
