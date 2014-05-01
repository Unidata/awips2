C MEMBER XTMPVL
C  (from old member PPXTMPVL)
C
      SUBROUTINE XTMPVL(PP24,NPCPN,AL24,PPVR,PTVR,NSTWT,AL6,MDR6,
     1 NBOX,BOXES,JONLY,JDIST,PPSR,MSNGSR,XC,YC,RI,MX,LHW,DATA,NFHW,
     2 N24,N6,NMR,NSR,KONLY,KDIST,IBUG,IERR)
C.......................................
C     THIS SUBROUTINE LOADS THE DATA VALUES NEEDED FOR THE CURRENT
C      DAY FOR A GIVEN MAP AREA INTO THE ARRAY TO BE WRITTEN TO THE
C      TEMPORARY FILE.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- MARCH 1983
C.......................................
      INTEGER*2 PP24(1),PPVR(1),PTVR(1),MDR6(1),PPSR(1),DATA(1)
      INTEGER*2 MDR24,MDRSIX(4),MSNGSR
      DIMENSION AL24(1),AL6(1),BOXES(1)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDEBUG,PDBUG(20),IPALL
      COMMON/XTIME/KZDA,KDA,KHR,LSTMO,KMO,KID,KYR,KIH,TZCODE,ISW,
     1  IUTMP,NSSR,IDAY
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xtmpvl.f,v $
     . $',                                                             '
     .$Id: xtmpvl.f,v 1.1 1995/09/17 19:00:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA TMPL/4HTMPL/
C.......................................
C     CHECK TRACE LEVEL
C
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
  900 FORMAT(1H0,17H** XTMPVL ENTERED)
C.......................................
C     INITIAL VALUES
      N24=0
      N6=0
      NMR=0
      NSR=0
      KONLY=0
      KDIST=0
      IERR=0
C.......................................
C     MRRONLY SECTION--STORE 6 AND 24 HOUR DATA IF NONE MISSING
      IF(JONLY.EQ.0) GO TO 110
      NP=KHR/6
      IF (NP*NBOX.LE.LHW) GO TO 101
      J=(NP*NBOX-LHW-1)/2+1
      CALL XSPACE(MX,J,TMPL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
  101 DO 100 I=1,NBOX
      MDRBOX=BOXES(I)
      CALL XPPMDR(MDRBOX,MDR6,KHR,MDR24,MDRSIX,MSG,ISTAT)
      IF((MSG.EQ.1).OR.(ISTAT.EQ.1)) GO TO 110
      K=(I-1)*NP
      DO 105 J=1,NP
  105 DATA(K+J)=MDRSIX(J)
  100 CONTINUE
      KONLY=1
      NMR=NBOX
      NFHW=NP*NBOX
      GO TO 90
C.......................................
C     24 HOUR PRECIPITATION SECTION
  110 IF(NPCPN.LE.LHW) GO TO 111
      J=(NPCPN-LHW-1)/2+1
      CALL XSPACE(MX,J,TMPL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
  111 DO 115 I=1,NPCPN
      J=AL24(I)
      J=J/5+1
  115 DATA(I)=PP24(J)
      N24=NPCPN
      NFHW=NPCPN
C.......................................
C     STRANGER REPORT SECTION
C     FOR NOW STRANGER REPORTS CANNOT BE USED IN AREA COMPUTATIONS.
      IF (NSSR.GT.0) GO TO 130
      IF((NSSR.EQ.0).OR.(RI.LE.0.0)) GO TO 130
      DO 120 I=1,NSSR
      J=(I-1)*3
      IF(PPSR(J+3).EQ.MSNGSR) GO TO 120
      YS=PPSR(J+1)*0.1
      XS=PPSR(J+2)*0.1
      DELX=XS-XC
      DELY=YS-YC
      D=SQRT(DELX*DELX+DELY*DELY)
      IF(D.GT.RI) GO TO 120
      IF(NFHW+3.LE.LHW) GO TO 125
      K=(NFHW+2-LHW)/2+1
      CALL XSPACE(MX,K,TMPL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
  125 DO 126 K=1,3
  126 DATA(NFHW+K)=PPSR(J+K)
      NFHW=NFHW+3
      NSR=NSR+1
  120 CONTINUE
C.......................................
C     MDRDIST SECTION--STORE 6 HOUR DATA IF NONE MISSING
  130 IF(JDIST.EQ.0) GO TO 140
      NP=KHR/6
      IF(NFHW+NP*NBOX.LE.LHW) GO TO 136
      J=(NFHW+NP*NBOX-LHW-1)/2+1
      CALL XSPACE(MX,J,TMPL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
  136 DO 135 I=1,NBOX
      MDRBOX=BOXES(I)
      CALL XPPMDR(MDRBOX,MDR6,KHR,MDR24,MDRSIX,MSG,ISTAT)
      IF((MSG.EQ.1).OR.(ISTAT.EQ.1)) GO TO 140
      DO 137 J=1,NP
      K=NFHW+(I-1)*NP
  137 DATA(K+J)=MDRSIX(J)
  135 CONTINUE
      KDIST=1
      NMR=NBOX
      NFHW=NFHW+NP*NBOX
      GO TO 90
C.......................................
C     SIX HOUR PRECIPITATION SECTION
  140 NP=KHR/6
      K=NSTWT*NP
      IF(NFHW+K.LE.LHW) GO TO 141
      J=(NFHW+K-LHW-1)/2+1
      CALL XSPACE(MX,J,TMPL)
      CALL KILLFN(8HMAP     )
      IERR=1
      GO TO 99
  141 DO 145 I=1,NSTWT
      J=AL6(I)
      K=PTVR(J+3)-1
      L=NFHW+(I-1)*NP
      DO 146 J=1,NP
  146 DATA(L+J)=PPVR(K+J)
  145 CONTINUE
      N6=NSTWT
      NFHW=NFHW+NP*NSTWT
C.......................................
C     CHECK FOR DEBUG OUTPUT
   90 IF(IBUG.EQ.0) GO TO 99
      WRITE(IOPDBG,901)KONLY,KDIST,N24,N6,NMR,NSR,NFHW
  901 FORMAT(1H ,7I5)
      WRITE(IOPDBG,902)(DATA(I),I=1,NFHW)
  902 FORMAT(1H ,25I5)
C.......................................
C     CHECK TRACE LEVEL
   99 IF(IPTRCE.GE.3) WRITE(IOPDBG,903)
  903 FORMAT(1H0,14H** EXIT XTMPVL)
      RETURN
      END
