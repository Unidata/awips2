      SUBROUTINE ADJTS21(IJ,J,NQSL,SLICE,FRMSO,FBIASO,RRMSO,RBIASO,
     . STE,IRF,FRMS,FBIAS,RRMS,RBIAS,STC,QTC,STT,KTIM,JNK)

C  THIS SUBROUTINE ADJUSTS THE COMPUTED STAGES AT GAGE LOCATIONS TO
C  CLOSER MATCH THE OBSERVED VALUES.  THIS IS A COSMETIC CHANGE ONLY.
C  THE RESULTS ARE ONLY AFFECTED AT THE GAGE LOCATIONS.  THE STATISTICS
C  ARE COMPUTED BY THREE METHODS:
C        1) TOTAL RMS- keep long-term statistics
C        2) RUNTIME RMS- use the statistic for the total run period
C                        if no RMS in this period, use TOTAL RMS
C        3) MOST RECENT RISE/FALL- go back to cycles and do statistics <== not used
C                        if no RMS in this period, use RUNTIME RMS

      INCLUDE 'common/fdbug'
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FNOPR/NOPROT
      COMMON/NSLC21/NSLICE

      DIMENSION STT(*),STC(*),QTC(*),STE(*),IRF(*),SLICE(*)
      DIMENSION FRMS(3,*),FBIAS(3,*),RRMS(3,*),RBIAS(3,*),NQSL(*)
      DIMENSION FRMSO(*),FBIASO(*),RRMSO(*)
      DIMENSION RBIASO(*)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_dwoper/RCS/adjts21.f,v $
     . $',                                                             '
     .$Id: adjts21.f,v 1.3 2000/09/27 16:05:41 page Exp $
     . $' /
C    ===================================================================
C
CC      DIMENSION STE2(5000)

      DATA  SNAME / 'ADJTS21 ' /
C
C
      CALL FPRBUG(SNAME,1,21,IBUG)
C
C  find the end of the observed data
      KEND=0
      KTIME=KTIM-1
      DO 10 KK=1,KTIME
        K=KTIME-KK+1
        IF(STT(K).LT.-900.) GO TO 10
        KEND=K
        GO TO 15
   10 CONTINUE
      KEND=KTIME

C...set RMS and BIAS to overall values
   15 DO 20 L=1,NSLICE
        FRMS(1,L)=FRMSO(L)
        FBIAS(1,L)=FBIASO(L)
        RRMS(1,L)=RRMSO(L)
        RBIAS(1,L)=RBIASO(L)
   20 CONTINUE

        DO 25 L=1,NSLICE
          DO 22 M=2,3
            FRMS(M,L)=0.
            FBIAS(M,L)=0.
            RRMS(M,L)=0.
            RBIAS(M,L)=0.
   22     CONTINUE
   25   CONTINUE

C  if no observed data in the period, then no adjustment is made except
C  with accum stats.
      IF(KEND.LT.6) THEN
        DO 30 K=1,KTIME
          IF(NQSL(J).EQ.0) THEN
            STE(K)=STC(K)
          ELSE
            STE(K)=QTC(J)
          ENDIF
   30   CONTINUE
        GO TO 500
      ENDIF

C  determine rises (IRF=1) and falls (IRF=0) for the computed stage
C  hydrograph
   35 IRF(1)=0
      IF(STC(1).LT.STC(2)) IRF(1)=1
      DO 40 K=2,KTIME
        IRF(K)=0
        IF(STC(K-1).LT.STC(K)) IRF(K)=1
   40 CONTINUE

C  determine the beginning point if going back two cycles
      IRSFL=IRF(KEND)
      IRISFAL=IRSFL
      ICOUNT=0
      KND=KEND-1
      IRS=0
      IFL=0

      DO 45 KK=1,KND
        K=KND-KK+1
        IF(IRF(K).EQ.IRF(K+1)) GO TO 45
        IF(IRISFAL.EQ.0.AND.IRF(K).EQ.1) THEN
          IRS=IRS+1
        ELSEIF(IRISFAL.EQ.1.AND.IRF(K).EQ.0) THEN
          IFL=IFL+1
        ENDIF
        IRISFAL=IRF(K)
        IF(IRSFL.EQ.0.AND.IFL.EQ.2) THEN
          ISTRT=K
          GO TO 47
        ELSEIF(IRSFL.EQ.1.AND.IRS.EQ.2) THEN
          ISTRT=K
          GO TO 47
        ENDIF
   45 CONTINUE
      ISTRT=1

C  find the stats for each slice
   47 DO 100 L=2,NSLICE
        LIJ=L+IJ-1
        NF=0
        TDIFF=0.
        TSDIFF=0.
        NR=0
        TDIFR=0.
        TSDIFR=0.
        NF2=0
        TDIFF2=0.
        TSDIFF2=0.
        NR2=0
        TDIFR2=0.
        TSDIFR2=0.

        IEND=0
        IEND2=0
CC        WRITE(IPR,52) L,SLICE(LIJ-1),SLICE(LIJ)
        IF(IBUG.EQ.1) WRITE(IPR,52) L,SLICE(L-1),SLICE(L)
   52   FORMAT(/20X,'SLICE NO.',I3,' BETWEEN',F12.1,' AND',F12.1
     .         /24X,'K',9X,'STT',9X,'STC',6X,'FDIF',6X,'RDIF')
        DO 50 K=1,KEND 
          IF(STT(K).LT.-900.) GO TO 50
          IEND=IEND+1
          IF(K.GE.ISTRT) IEND2=IEND2+1
          IF(NQSL(J).EQ.0) THEN
            STHQ=STC(K)
          ELSE
            STHQ=QTC(K)*1000.
          ENDIF
          IRANG=0
          IF(STHQ.GE.SLICE(L-1).AND.STHQ.LE.SLICE(L)) IRANG=1
          IF(L.EQ.NSLICE.AND.STHQ.GE.SLICE(L)) IRANG=1
          IF(IRANG.EQ.1) THEN
            DIF=STC(K)-STT(K)
            SDIF=DIF**2
            IF(IRF(K).EQ.0) THEN
              NF=NF+1
              TDIFF=TDIFF+DIF
              TSDIFF=TSDIFF+SDIF
              IF(K.GE.ISTRT) THEN
                NF2=NF2+1
                TDIFF2=TDIFF2+DIF
                TSDIFF2=TSDIFF2+SDIF
              ENDIF
              IF(IBUG.EQ.1) WRITE(IPR,54) K,STT(K),STC(K),DIF
   54         FORMAT(20X,I5,2F12.3,F10.3)
            ELSE
              NR=NR+1
              TDIFR=TDIFR+DIF
              TSDIFR=TSDIFR+SDIF
              IF(K.GE.ISTRT) THEN
                NR2=NR2+1
                TDIFR2=TDIFR2+DIF
                TSDIFR2=TSDIFR2+SDIF
              ENDIF
              IF(IBUG.EQ.1) WRITE(IPR,56) K,STT(K),STC(K),DIF
   56         FORMAT(20X,I5,2F12.3,10X,F10.3)
            ENDIF
          ENDIF
   50   CONTINUE
        IF(NF.GT.0) THEN
          FRMS(2,L)=SQRT(TSDIFF/NF)
          FBIAS(2,L)=TDIFF/NF
          IF(FBIAS(2,L).GT.0) FRMS(2,L)=-FRMS(2,L)
c...use the overall statistics
        ELSE
          FRMS(2,L)=FRMS(1,L)
          FBIAS(2,L)=FBIAS(1,L)
        ENDIF
        IF(NR.GT.0) THEN
          RRMS(2,L)=SQRT(TSDIFR/NR)
          RBIAS(2,L)=TDIFR/NR
          IF(RBIAS(2,L).GT.0) RRMS(2,L)=-RRMS(2,L)
        ELSE
c...use the overall statistics
          RRMS(2,L)=RRMS(1,L)
          RBIAS(2,L)=RBIAS(1,L)
        ENDIF
        IF(NF2.GT.0) THEN
          FRMS(3,L)=SQRT(TSDIFF2/NF2)
          FBIAS(3,L)=TDIFF2/NF2
          IF(FBIAS(3,L).GT.0) FRMS(3,L)=-FRMS(3,L)
        ELSE
          FRMS(3,L)=FRMS(2,L)
          FBIAS(3,L)=FBIAS(2,L)
        ENDIF
        IF(NR2.GT.0) THEN
          RRMS(3,L)=SQRT(TSDIFR2/NR2)
          RBIAS(3,L)=TDIFR2/NR2
          IF(RBIAS(3,L).GT.0) RRMS(3,L)=-RRMS(3,L)
        ELSE
          RBIAS(3,L)=RBIAS(2,L)
          RBIAS(3,L)=RBIAS(2,L)
        ENDIF

C  make adjustments to points within the slice
        DO 60 K=1,KTIME
          IF(NQSL(J).EQ.0) THEN
            STHQ=STC(K)
          ELSE
            STHQ=QTC(K)*1000.
          ENDIF
          IRANG=0
          IF(STHQ.GE.SLICE(L-1).AND.STHQ.LE.SLICE(L)) IRANG=1
          IF(L.EQ.NSLICE.AND.STHQ.GE.SLICE(L)) IRANG=1
          IF(IRANG.EQ.1) THEN
            IF(IRF(K).EQ.0) THEN
              STE(K)=STC(K)+FRMS(2,L)
CC              STE2(K)=STC(K)+FRMS(3,L)
            ENDIF
            IF(IRF(K).EQ.1) THEN
              STE(K)=STC(K)+RRMS(2,L)
CC              STE2(K)=STC(K)+RRMS(3,L)
            ENDIF
          ENDIF
   60   CONTINUE
  100 CONTINUE
  500 IF(IBUG.EQ.1) WRITE(IPR,105)
CC  105 FORMAT(9X,'K       STT       STC      STE2      STE3')
  105 FORMAT(9X,'K       STT       STC       STE')
      DO 115 ,K=1,KTIME
CC        WRITE(IPR,110) K,STT(K),STC(K),STE(K),STE2(K)
        IF(IBUG.EQ.1) WRITE(IPR,110) K,STT(K),STC(K),STE(K)
  110   FORMAT(5X,I5,4F10.2)
  115 CONTINUE
      OSUMN=0.
      OSUMN2=0.
      SUMN=0.
      SUMN2=0.
      SUMNN=0.
      SUMNN2=0.
      NU2=KEND-ISTRT+1
      DO 120 K=1,KEND
        IF(STT(K).LT.-900.) GO TO 120
        ODIFN=STC(K)-STT(K)
        ODIFN2=ODIFN**2
        OSUMN=OSUMN+ODIFN
        OSUMN2=OSUMN2+ODIFN2
        DIFN=STE(K)-STT(K)
        DIFN2=DIFN**2
        SUMN=SUMN+DIFN
        SUMN2=SUMN2+DIFN2
        IF(K.GE.ISTRT) THEN
          DIFNN=STE(K)-STT(K)
          DIFNN2=DIFNN**2
          SUMNN=SUMNN+DIFNN
          SUMNN2=SUMNN2+DIFNN2
        ENDIF
  120 CONTINUE
      ORMSN=SQRT(OSUMN2/IEND)
      RMSN=SQRT(SUMN2/IEND)
      RMSNN=SQRT(SUMNN2/IEND2)
      OBIASN=OSUMN/IEND
      BIASN=SUMN/IEND
      BIASNN=SUMNN/IEND2
cc      WRITE(IPR,130) ORMSN,OBIASN,RMSN,BIASN,RMSNN,BIASNN
      IF(IBUG.EQ.1) WRITE(IPR,130) ORMSN,OBIASN,RMSN,BIASN
  130 FORMAT(//5X,'OLD RMS=',F8.4,5X,'OLD BIAS=',F8.4,2X,'ORIGINAL RMS'
     .        /5X,'NEW RMS=',F8.4,5X,'NEW BIAS=',F8.4,2X,'RUNTIME RMS')
cc     .        /5X,'NEW RMS=',F8.4,5X,'NEW BIAS=',F8.4,2X,'RECENT RMS')
      IF(JNK.GE.1.AND.NOPROT.EQ.0) THEN
        WRITE(IPR,132) (SLICE(L),L=1,NSLICE)
  132   FORMAT(/2X,'SLICES:',20F10.0)
        WRITE(IPR,134) (FRMS(3,L),L=1,NSLICE)
  134   FORMAT(2X,'FRMS:  ',20F10.4)
        WRITE(IPR,136) (FBIAS(3,L),L=1,NSLICE)
  136   FORMAT(2X,'FBIAS: ',20F10.4)
        WRITE(IPR,138) (RRMS(3,L),L=1,NSLICE)
  138   FORMAT(2X,'RRMS:  ',20F10.4)
        WRITE(IPR,140) (RBIAS(3,L),L=1,NSLICE)
  140   FORMAT(2X,'RBIAS: ',20F10.4)
      ENDIF
      IF(ITRACE.EQ.1) WRITE(IODBUG,9000) SNAME
 9000 FORMAT(1H0,'** ',A,' EXITED.')
      RETURN
      END
