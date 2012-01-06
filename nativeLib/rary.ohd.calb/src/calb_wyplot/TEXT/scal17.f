C MEMBER SCAL17
C  (from old member MCEX17)
C
C @PROCESS LVL(77) OPT(2)
C
      SUBROUTINE SCAL17(IS,LRO,LSM,SMTYPE,KMO,KYR,MN,SCALE,CMO)
C.......................................
C     THIS SUBROUTINE PRINTS THE SCALES FOR THE 'WY-PLOT ' OPERATION.
C        ALSO SOIL-MOISTURE STORAGE HEADINGS IF NEEDED.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL     APRIL 1980
C     MODIFIED TO PRINT YEAR IN SCALE HEADING
C        GEORGE F. SMITH - HRL   JUNE 1986
C.......................................
C
C     VARIABLES IN THE ARGUMENT LIST - SCAL17
C
C        IS      -  PLOT SCALE INDICATOR (INPUT)
C        LRO     -  RUNOFF COMPONENTS SWITCH (INPUT)
C        LSM     -  SOIL-MOISTURE STORAGES SWITCH (INPUT)
C        KMO     -  CURRENT MONTH (INPUT)
C        KYR     -  CURRENT YEAR (INPUT)
C        MN      -  CONTROL SWITCH (INPUT)
C                     =1  PRINT SCALE
C                     =2  PRINT SOIL-MOISTURE STORAGE HEADING
C        SCALE   -  PLOT SCALE VALUES (INPUT)
C        CMO     -  CHARACTER REPRESENTATION FOR EACH MONTH (INPUT)
C
C.......................................
      DIMENSION SCALE(10),CMO(12),UNITL(2)
C
C     COMMON BLOCKS.
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/scal17.f,v $
     . $',                                                             '
     .$Id: scal17.f,v 1.2 1996/07/11 19:43:20 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     DATA STATEMENTS
      DATA UNITL/4HIN  ,4HMM  /
      DATA APIC/4HAPIC/
C.......................................
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,10)
10    FORMAT (1H0,17H** SCAL17 ENTERED)
C.......................................
      IF(MN.EQ.2) GO TO 120
C.......................................
C     PRINT SCALE
      M1=KMO
      M2=KMO+1
      KY1=KYR
      KY2=KYR
      IF(M2.LT.13)GO TO 20
      M2=M2-12
      KY2=KY2+1
20    IF(LRO.GT.0) GO TO 70
C
C     NO RUNOFF COMPONENTS
      WRITE(IPR,30) CMO(M1),KY1,CMO(M2),KY2
30    FORMAT(1H ,2X,A3,2H, ,I4,3H - ,A3,2H, ,I4)
      IF(IS.GT.1) GO TO 50
C     SEMI-LOG PLOT
      WRITE(IPR,40) (SCALE(I),I=1,5)
40    FORMAT(1H ,3HDAY,2X,3HPCN,3X,4HQ(1),3X,4HQ(2),14X,F10.3,
     14(10X,F10.2))
      RETURN
C     ARITHMETIC OR MODIFIED
50    WRITE(IPR,60) SCALE
60    FORMAT(1H ,3HDAY,2X,3HPCN,3X,4HQ(1),3X,4HQ(2),4X,10F10.1)
      RETURN
C
C     RUNOFF COMPONENTS INCLUDED.
70    WRITE(IPR,80) CMO(M1),KY1,CMO(M2),KY2
80    FORMAT(1H ,2X,A3,2H, ,I4,3H - ,A3,2H, ,I4,89X,3HPRM,3X,3HIMP,3X,
     1 3HSUR)
      IF(IS.GT.1) GO TO 100
C     SEMI-LOG PLOT
      WRITE(IPR,90) (SCALE(I),I=1,4)
90    FORMAT(1H ,3HDAY,2X,3HPCN,3X,4HQ(1),3X,4HQ(2),4X,F10.3,
     13(10X,F10.2),11X,4HINFW,4X,3HSUP,3X,3HDIR,3X,3HINT)
      RETURN
C     ARITHMETIC OR MODIFIED
100   WRITE(IPR,110) (SCALE(I),I=1,8)
110   FORMAT(1H ,3HDAY,2X,3HPCN,3X,4HQ(1),3X,4HQ(2),4X,7F10.1,
     1F9.0,2X,4HINFW,4X,3HSUP,3X,3HDIR,3X,3HINT)
      RETURN
C.......................................
C     SOIL-MOISTURE STORAGE HEADINGS.
120   IF(LSM.EQ.0) RETURN
      IF(LRO.EQ.0) GO TO 180
      IF (SMTYPE.EQ.APIC) GO TO 140
      WRITE(IPR,130)
130   FORMAT(1H ,30X,36HUZTD AND LZTD=TENSION WATER DEFICITS,15X,
     124HUZTD UZFW LZTD LZFS LZFP)
      GO TO 160
140   WRITE(IPR,150)
150   FORMAT(1H ,82X,22HAPI   AI  SMI BFSC BFI)
160   WRITE(IPR,170) UNITL(METRIC+1)
170   FORMAT(1H ,87X,9HUNITS ARE,1X,A4)
      RETURN
180   IF (SMTYPE.EQ.APIC) GO TO 200
      WRITE(IPR,190)
190   FORMAT(1H ,50X,36HUZTD AND LZTD=TENSION WATER DEFICITS,15X,
     124HUZTD UZFW LZTD LZFS LZFP)
      GO TO 220
200   WRITE(IPR,210)
210   FORMAT(1H ,102X,22HAPI   AI  SMI BFSC BFI)
220   WRITE(IPR,230) UNITL(METRIC+1)
230   FORMAT(1H ,107X,9HUNITS ARE,1X,A4)
C.......................................
      RETURN
      END
