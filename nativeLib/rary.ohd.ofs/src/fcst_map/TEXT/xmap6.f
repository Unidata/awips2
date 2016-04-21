C MEMBER XMAP6
C  (from old member PPXMAP6)
C
      SUBROUTINE XMAP6(NP,DATA,NSKIP,N6,TWTS,TMAP,TSMAP)
C.......................................
C     THIS SUBROUTINE USES SIX HOUR STATION NORMALIZED VALUES
C        TO TIME DISTRIBUTE A 24 HOUR MAP VALUE.
C.......................................
C     WRITTEN BY -- ERIC ANDERSON, HRL -- MARCH 1983
C.......................................
      INTEGER*2 DATA(1),MSNG24,MSNG6,MSGMDR,MSNGSR
      DIMENSION TWTS(1),TSMAP(1),TOP(4)
C
C     COMMON BLOCKS
      COMMON/PUDBUG/IOPDBG,IPTRCE,NDBUG,PDBUG(20),IPALL
      COMMON/XSIZE/NMAP,NDUPL,NSLT24,NSLOT6,LDATA6,LMDR,LPPSR,
     1 MSNG24,MSNG6,MSGMDR,MSNGSR,NRECTP,MXTEMP,SMALL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_map/RCS/xmap6.f,v $
     . $',                                                             '
     .$Id: xmap6.f,v 1.1 1995/09/17 18:59:43 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     CHECK TRACE LEVEL
      IF(IPTRCE.GE.3) WRITE(IOPDBG,900)
  900 FORMAT(1H0,16H** XMAP6 ENTERED)
C.......................................
C     COMPUTE WEIGHTING FACTORS FOR EACH PERIOD
      SWT=0.0
      DO 100 I=1,NP
  100 TOP(I)=0.0
      DO 110 N=1,N6
      J=NSKIP+(N-1)*NP+1
      IF(DATA(J).EQ.MSNG6) GO TO 110
      SWT=SWT+TWTS(N)
      DO 120 I=1,NP
      J=NSKIP+(N-1)*NP+I
      XN=DATA(J)*0.01
  120 TOP(I)=TOP(I)+XN*TWTS(N)
  110 CONTINUE
      IF(SWT.GT.5.0E-8) GO TO 140
C.......................................
C     USE UNIFORM DISTRIBUTION
      F=1.0/NP
      DO 130 I=1,NP
  130 TSMAP(I)=F*TMAP
      GO TO 99
C.......................................
C     COMPUTE 6 HR MAP FROM NORMALIZED VALUES.
  140 DO 150 I=1,NP
  150 TSMAP(I)=(TOP(I)/SWT)*TMAP
C.......................................
C     CHECK TRACE LEVEL
   99 IF(IPTRCE.GE.3) WRITE(IOPDBG,901)
  901 FORMAT(1H0,13H** EXIT XMAP6)
      RETURN
      END
