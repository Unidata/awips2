C MEMBER XPPMDR
C  (from old member PPXPPMDR)
C
      SUBROUTINE XPPMDR(MDRBOX,MDR6,KHR,MDR24,MDRSIX,MSG,ISTAT)
C.......................................
C     SUBROUTINE RETURNS THE 6 HOUR VALUES AND THE 24 HOUR
C      SUM OF THE MDR PRECIPATION ESTIMATES FOR A GIVEN NATIONAL
C      MDR BOX.
C.......................................
C     WRITTEN BY--ERIC ANDERSON,HRL--FEBRUARY 1983
C.......................................
      INTEGER*2 MDR6(1),MDR24,MDRSIX(4),MSNG24,MSNG6,MSGMDR,MSNGSR
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XMDR/MDRSTA,ICTMDR,MDRST6,ICMDR,NCMDR,IRMDR,NRMDR,
     1NMDR,MDR,MDRNUV
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1   MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xppmdr.f,v $
     . $',                                                             '
     .$Id: xppmdr.f,v 1.1 1995/09/17 19:00:08 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C      CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
 900  FORMAT(1H0,17H** XPPMDR ENTERED)
C.......................................
C     GET MDR SUBSET ARRAY LOCATION
      CALL XMDRAL(MDRBOX,LOC,ISTAT)
      IF(ISTAT.EQ.1) GO TO 190
C.......................................
C     GET MDR VALUES
      MDR24=0
      MSG=0
      NP=KHR/6
      DO 100 I=1,NP
      J=(I-1)*NMDR+LOC
      IF (MDR6(J).EQ.MSNG6) GO TO 105
      MDRSIX(I)=MDR6(J)
      MDR24=MDR24+MDRSIX(I)
      GO TO 100
 105  MSG=1
      MDRSIX(I)=MSNG6
 100  CONTINUE
      IF(MSG.EQ.0) GO TO 99
      GO TO 195
C.......................................
C     MDR BOX NOT IN SUBSET
 190  NP=KHR/6
      MSG=1
      DO 191 I=1,NP
 191  MDRSIX(I)=MSNG6
 195  MDR24=MSNG24
C.......................................
  99  IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
 901  FORMAT(1H0,14H** EXIT XPPMDR)
      RETURN
      END
