C MEMBER XPLT24
C  (from old member PPXPLT24)
C
      SUBROUTINE XPLT24(VAL,IND,PLAT,PLON,IEST,IPC,ISW,STA)
C.......................................
C     SUBROUTINE WRITES INFORMATION FOR A STATION WITH NON-ZERO
C         PRECIPITATION TO A TEMPORARY FILE SO THAT AN AFOS PLOT
C         CAN BE GENERATED.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON,HRL    NOVEMBER 1983
C.......................................
      INTEGER*2 VAL
      DIMENSION STA(2)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XDISPL/MDRPRT,MDRCOM,IPRT24,IPLT24,IPRT6,IPRSSR,
     1   IPRSRW,MAPPRT,METRIC,LASTDY,IPRDAY,IPLTMP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xplt24.f,v $
     . $',                                                             '
     .$Id: xplt24.f,v 1.1 1995/09/17 19:00:04 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA BNK,CC,CZ,CM,CE/1H ,1HC,1HZ,1HM,1HE/
      DATA CG/1HG/
C.......................................
C     CHECK TRACE LEVEL
      IF (IPTRCE.GE.3) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** XPLT24 ENTERED)
C.......................................
C     SET ESTIMATION CODE
      IF (IEST) 105,110,115
  105 CODE=CZ
      GO TO 100
  110 CODE=BNK
      IF (IND.EQ.6) GO TO 111
      ICF=IPC
      GO TO 113
  111 IF (IPC.LT.0) IPC=-IPC
      IW=IPC/100
      IS=IPC-IW*100
      IF (IS.EQ.0) GO TO 112
      IF (ISW.EQ.1) GO TO 112
      ICF=IS*5
      GO TO 113
  112 ICF=IW*5
  113 IF (ICF.NE.100) CODE=CC
      GO TO 100
  115 J=IEST-2
      IF (J) 116,120,121
  116 CODE=CE
      GO TO 100
  120 CODE=CM
      GO TO 100
  121 CODE=CG
C.......................................
C     WRITE INFORMATION TO TEMPORARY FILE
  100 WRITE(IPLTMP) VAL,IND,PLAT,PLON,CODE,STA
C.......................................
      IF (IPTRCE.GE.3) WRITE(IOPDBG,901)
  901 FORMAT(1H0,14H** EXIT XPLT24)
      RETURN
      END
