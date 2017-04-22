C MODULE WRTITL55
C-----------------------------------------------------------------------
C
      SUBROUTINE WRTITL55 (NB,NBT,X,XT,FLDSTG,TYPK,YPK,
     . TQPK,QPK,NGAGE,NGS,GZ,STTNAM,STT,LTSTT,STQ,LTSTQ,T1,LTT1,
     . RMS,AVD,NN,MRV,NJUN,KRCH,HS,BS,BSS,BSL,BSR,SYSTM,RIVR,KMO,
     . KDA,KYR,KHR,ZONE,K1,K2,K4,K9,K23,K28)

      CHARACTER*4 SYSTM(20),RIVR(20,K1),RIVER(4),BLANK,ZONE

      COMMON/IONUM/IN,IPR,IPU
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/FLP55/KFLP
      COMMON/METR55/METRIC
      INCLUDE 'common/opfil55'

      DIMENSION STT(*),LTSTT(*),STQ(*),LTSTQ(*),T1(*),LTT1(*)
      DIMENSION NB(K1),NBT(K1),HS(K9,K2,K1),X(K2,K1),XT(K23,K1)
      DIMENSION TYPK(K2,K1),YPK(K2,K1),TQPK(K2,K1),QPK(K2,K1),NGS(K4,K1)
      DIMENSION FLDSTG(K23,K1),GZ(K4,K1),NGAGE(K1),STTNAM(5,K28)
      DIMENSION AVD(K4,K1),NN(K23,K1),RMS(K4,K1),MRV(K1),NJUN(K1)
      DIMENSION BS(K9,K2,K1),BSS(K9,K2,K1),BSL(K9,K2,K1),BSR(K9,K2,K1)
      DIMENSION KRCH(K2,K1),VALUE(500,2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/wrtitl55.f,v $
     . $',                                                             '
     .$Id: wrtitl55.f,v 1.7 2004/02/02 21:50:38 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/'    ' /
C
C
      NDXM=0
      MXVL=500

      WRITE(JFTTL,100) (SYSTM(L),L=1,10)
  100 FORMAT(10A4)
      IEMPTY=1

      DO 104 L=1,4
      RIVER(L)=RIVR(L,1)
 104  CONTINUE

      IEMPTY=0
CC  105 IF(RIVER(.EQ.BLANK) RIVER='_____ RIVER'
      WRITE(JFTTL,'(4A4)') (RIVR(L,1),L=1,4)

CC      PRINT 99, JRIVR,NB(JRIVR),NBT(JRIVR)
CC   99 FORMAT(5X,'JRIVER=',I1,2X,'NB=',I5,2X,'NBT=',I5)
      NGAG=NGAGE(1)
      IF(IOBS.EQ.0) NGAG=0
      NFLNCS=0
      IF(KFLP.GT.0) NFLNCS=NCS
      WRITE(JFTTL,110) NB(1),NUMTIM,NBT(1),NCS,NFLNCS,JN,NGAG,METRIC,
     .  KYR,KMO,KDA,KHR,ZONE
  110 FORMAT(8I5,2X,I4.4,3I2.2,A4)
      IF(JN.GT.1) THEN
        DO 150 J=2,JN
        NGAG=NGAGE(J)
        IF(IOBS.EQ.0) NGAG=0
cc        RIVER=BLANK
cc        IF(IEMPTY.EQ.0) READ(IN,'(A)',ERR=115) RIVER
        DO 30 L=1,4
        RIVER(L)=RIVR(L,J)
 30     CONTINUE
CC  115   IF(RIVER.EQ.BLANK) RIVER='______ RIVER'
        WRITE(JFTTL,120) NB(J),NBT(J),NGAG,RIVER
  120   FORMAT(3I5,1X,4A4)
        IF(NFGRF.EQ.2) WRITE(JFTRIB,125) J,MRV(J),NJUN(J)
  125   FORMAT(3I10)
  150   CONTINUE
      END IF

      DO 200 J=1,JN
      JRIVR=J
      N=NB(JRIVR)

cc        WRITE(JFHS) (HS(K,I,JRIVR),K=1,NCS)
cc        WRITE(JFBS) (BS(K,I,JRIVR),K=1,NCS)
cc        WRITE(JFBSS) (BSS(K,I,JRIVR),K=1,NCS)
cc        IF(KFLP.GT.0) THEN
cc          WRITE(JFBSL) (BSL(K,I,JRIVR),K=1,NCS)
cc         WRITE(JFBSR) (BSR(K,I,JRIVR),K=1,NCS)
cc        ENDIF

      IF(METRIC.EQ.1) THEN
        DO 201 I=1,N
        X(I,JRIVR)=X(I,JRIVR)*1.6093
        QPK(I,JRIVR)=QPK(I,JRIVR)/35.32
        YPK(I,JRIVR)=YPK(I,JRIVR)/3.281
        FLDSTG(I,JRIVR)=FLDSTG(I,JRIVR)/3.281
        HS(1,I,JRIVR)=HS(1,I,JRIVR)/3.281
        BS(1,I,JRIVR)=BS(1,I,JRIVR)/3.281
        BSS(1,I,JRIVR)=BSS(1,I,JRIVR)/3.281
        IF(KFLP.GT.0) THEN
          BSL(1,I,JRIVR)=BSL(1,I,JRIVR)/3.281
          BSR(1,I,JRIVR)=BSR(1,I,JRIVR)/3.281
        ENDIF
  201   CONTINUE
      ENDIF
      IF(NFGRF.EQ.0) WRITE(JFLOC) (X(I,JRIVR),I=1,N)
      NNJ=NBT(JRIVR)
      IF(METRIC.EQ.1) THEN
        DO 202 I=1,NNJ
        XT(I,JRIVR)=XT(I,JRIVR)*1.6093      
        FLDSTG(I,JRIVR)=FLDSTG(I,JRIVR)/3.281
  202 CONTINUE
      ENDIF
      IF(NFGRF.EQ.0) THEN
        II=NN(I,J)
        WRITE(JFXS) (XT(I,JRIVR),I=1,NNJ)
        WRITE(JFFLD) (FLDSTG(I,JRIVR),I=1,N)
      ENDIF
CC      II=NN(I,J)
cc      WRITE(JFFLD) (FLDSTG(NN(I,JRIVR),JRIVR),I=1,NNJ)

      IF(NFGRF.EQ.2) THEN
        I1=2
        WRITE(JFDIST,205) X(1,JRIVR),'Y'
  205   FORMAT(F12.3,1X,A1)
        WRITE(JFFLOOD,205) FLDSTG(1,JRIVR)
        DO 2010 I=2,NNJ
          I2=NN(I,J)-1
          IF(I1.LE.I2) THEN
            DO 2005 II=I1,I2
              WRITE(JFDIST,205) X(II,JRIVR),'N'
 2005       CONTINUE
          ENDIF
          WRITE(JFFLOOD,205) FLDSTG(I2+1,JRIVR)
          WRITE(JFDIST,205) X(I2+1,JRIVR),'Y'
          I1=I2+2
 2010   CONTINUE
      ENDIF

      DO 203 II=1,NNJ
        I=NN(II,JRIVR)
        WRITE(JFHS) (HS(K,I,JRIVR),K=1,NCS)
        WRITE(JFBS) (BS(K,I,JRIVR),K=1,NCS)
        WRITE(JFBSS) (BSS(K,I,JRIVR),K=1,NCS)
        IF(KFLP.GT.0) THEN
          WRITE(JFBSL) (BSL(K,I,JRIVR),K=1,NCS)
          WRITE(JFBSR) (BSR(K,I,JRIVR),K=1,NCS)
        ENDIF
  203 CONTINUE
      DO 160 I=1,N
CC        XRV=X(I,JRIVR)
CC       QP=QPK(I,JRIVR)
CC        TQP=TQPK(I,JRIVR)
CC        YP=YPK(I,JRIVR)
CC        TYP=TYPK(I,JRIVR)
CC        HS1=HS(1,I,JRIVR)
CC        IF(METRIC.EQ.1) THEN
CC          XRV=XRV*1.6093
CC          QP=QP/35.32
CC          YP=YP/3.281
CC          HS1/3.281
CC        ENDIF
CC        WRITE(JFPK) XRV,QP,TQP,YP,TYP,HS1
        IF(NFGRF.EQ.0) WRITE(JFPK) X(I,JRIVR),QPK(I,JRIVR),
     .   TQPK(I,JRIVR),YPK(I,JRIVR),TYPK(I,JRIVR),HS(1,I,JRIVR)

        IF(NFGRF.EQ.2) THEN
          IF(KRCH(I,JRIVR).GE.10) THEN
            WRITE(JFINTB,158) JRIVR,I,KRCH(I,JRIVR)
  158       FORMAT(3I10)
          ENDIF
        ENDIF
  160 CONTINUE

      NGAG=NGAGE(JRIVR)
      IF(NGAG.EQ.0.OR.KPL.EQ.0.OR.IOBS.LE.0) GO TO 200
      IF(NFGRF.EQ.0) THEN
        WRITE(JFOBS) KPL,NU
         LT1=LTT1(JRIVR)-1
        WRITE(JFOBS) (T1(K+LT1),K=1,NU)
      ENDIF
      IF(NFGRF.EQ.2)  WRITE(JFGAGE,125) KPL,NU
        NNJ=NBT(J)
        DO 180 L=1,NGAG
          I=NGS(L,JRIVR)
          LJ=LCAT21(L,JRIVR,NGAGE)
          GZERO=0.
          IF(KPL.NE.2) GZERO=GZ(L,JRIVR)
          RMSER=RMS(L,JRIVR)
          AVER=AVD(L,JRIVR)
          XRV=X(I,JRIVR)
          DO 162 K=1,NNJ
            IF(ABS(XT(K,JRIVR)-X(I,JRIVR)).LE.0.00001) GO TO 164
  162     CONTINUE
  164     FLST=FLDSTG(K,JRIVR)
          IF(METRIC.EQ.1) THEN
            IF(KPL.NE.2) GZERO=GZERO/3.281
          ENDIF
            IF(NFGRF.EQ.0) WRITE(JFGZ) XRV,GZERO,FLST,RMSER,AVER,
     .                 (STTNAM(K,LJ),K=1,3)
          IF(NFGRF.EQ.2) THEN
            WRITE(JFSTAT,165) (STTNAM(K,LJ),K=1,3)
  165       FORMAT(5A4)
            WRITE(JFSTAT,170) XRV,GZERO,FLST,RMSER,AVER
  170       FORMAT(5F12.3)
          ENDIF
          STTUNT=1.
          STQUNT=1.
          IF(METRIC.EQ.1) THEN
CC            DO 168 K=1,NU
CC              IF(KPL.NE.2) STT(K,L,JRIVR)=STT(K,L,JRIVR)/3.281
CC              IF(KPL.EQ.2) STT(K,L,JRIVR)=STT(K,L,JRIVR)/35.32
CC              IF(KPL.EQ.3) STQ(K,L,JRIVR)=STQ(K,L,JRIVR)/35.32
CC  168       CONTINUE
            STTUNT=3.281
            IF(KPL.EQ.2) STTUNT=35.32
            IF(KPL.EQ.3) STQUNT=35.32
        ENDIF
        
cc        LJ=LCAT21(L,JRIVR,NGAGE)
        LSTT=LTSTT(LJ)-1
        LSTQ=LTSTQ(LJ)-1
        do 169 k=1,nu
          sttval=STT(K+LSTT)
          if(sttval.gt.-990) sttval=sttval/STTUNT
cc          if(sttval.gt.-990) sttval=sttval/STTUNT+gzero
          IF(NFGRF.EQ.0) WRITE(JFOBS) sttval
 169    continue
        IF(KPL.EQ.3.AND.NFGRF.EQ.0) WRITE(JFOBS) 
     .       ((STQ(K+LSTQ)/STQUNT),K=1,NU)
  180 CONTINUE
      IF(NFGRF.EQ.2) THEN
        IF(NGAG.GT.MXVL) THEN
          WRITE(IPR,182)  NGAG,J,MXVL
  182     FORMAT(//5X,'****NOTE*** NUMBER OF GAGES (',I5,' ON RIVER',I3,
     .      ' GREATER THAN',I5,'.  FLDAT GAGE FILE NOT FILLED')
            RETURN
        ENDIF
        LT1=LTT1(JRIVR)-1
        DO 2025 K=1,NU
          DO 184 L=1,NGAG
            LJ=LCAT21(L,JRIVR,NGAGE)
            LSTT=LTSTT(LJ)-1
            LSTQ=LTSTQ(LJ)-1
            VALUE(L,1)=STT(K+LSTT)/STTUNT
            IF(KPL.EQ.3) VALUE(L,2)=STQ(K+LSTQ)/STQUNT
  184     CONTINUE
          IF(KPL.NE.3) WRITE(JFGAGE,185) T1(K+LT1),
     .        (VALUE(L,1),L=1,NGAG)
  185     FORMAT(5000(F13.3,1X))
          IF(KPL.EQ.3) WRITE(JFGAGE,185) T1(K+LT1),
     .        (VALUE(L,1),VALUE(L,2),L=1,NGAG)
 2025   CONTINUE
      ENDIF
  200 CONTINUE
C
C  CLOSE FILES
      IUNIT=0
      if(nfgrf.eq.2) then
        CALL CLFILE ('FLDWAV-FLDAT',IUNIT,IERR)
      else
        CALL CLFILE ('FLDWAV-FLDGRF',IUNIT,IERR)
      endif
C
      RETURN
C
      END





