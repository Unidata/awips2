C MODULE MEPCON
C-----------------------------------------------------------------------
C
      SUBROUTINE MEPCON (NSTA,NMO,PXNAME,IMO,IYR,DUNITS,IMOW,IMOS,
     *   PT_FLAG)
C
C  ROUTINE TO DO CONSISTENCY CHECKS
C
      CHARACTER*20 PXNAME(*)
C
      DIMENSION NPG(5),IGS(5,25),NPLUS(5),IGROUP(25)
      DIMENSION BASE(5,600),BSTA(5),ACCUM(5),IGN(5),S(11)
      DIMENSION SYM(5)
C
      INCLUDE 'uiox'
      COMMON /MEAPPX/ APPE(25,600)
      COMMON /MEPLTX/ ORD(101,101)
C
C  DEFINE NAME OF FILE TO RECEIVE DATA NEEDED FOR IDMA
      CHARACTER*132 NEWNAM
C
C  DEFINE FLAG FOR WHETHER MONTHLY STATION TIMESERIES PE DATA
C  AVAILABLE (0), ESTIMATED (1), OR MISSING (2)
      INTEGER*2 PT_FLAG(25,600), IFLAG(5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/mepcon.f,v $
     . $',                                                             '
     .$Id: mepcon.f,v 1.4 2002/02/11 18:35:35 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK,DOT,ASTER/1H ,1H.,1H*/
      DATA SYM/1H1,1H2,1H3,1H4,1H5/
C
C
C  SET UNIT NUMBER FOR IDMA OUTPUT
      LIDMA=99
C
C  SET FILE NAME FOR IDMA OUTPUT
      CALL USUFIT (LP,'_dma',NEWNAM,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,385) 'USUFIT',ISTAT
         CALL UWARN (LP,0,-1)
         LIDMA=0
         ENDIF
C
      IF (LIDMA.GT.0) THEN
C     OPEN IDMA OUTPUT FILE
         CALL UPOPEN (LIDMA,NEWNAM,0,'F',ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,385) 'UPOPEN',ISTAT
            CALL UWARN (LP,0,-1)
            ENDIF
         ENDIF
C
      READ (ICD,380,END=370) ICNCOP,NG,(NPG(IG),IG=1,NG)
C
      IF (ICNCOP.GT.0.AND.ICNCOP.LT.3) GO TO 10
C
      IF (NG.GT.0.OR.NPG(1).GT.0) WRITE (LP,390) ICNCOP,NG,NPG(1)
      CALL UWARN (LP,0,-1)
      WRITE (LP,400) ICNCOP
      ICNCOP=0
      GO TO 370
C
10    LMOW=IMOS-1
      LMOS=IMOW-1
C
      DO 30 IG=1,NG
         NUM=NPG(IG)
         READ (ICD,380) (IGS(IG,I),I=1,NUM)
         DO 20 I=1,NUM
            N=IGS(IG,I)
            N=IABS(N)
            IGROUP(N)=IG
20          CONTINUE
30       CONTINUE
C
      IG=1
C
40    NNZ=0
      NP=0
      NUM=NPG(IG)
      DO 50 I=1,NUM
         N=IGS(IG,I)
C     COUNT THE TOTAL NUMBER OF STATIONS
         NNZ=NNZ+1
C     COUNT THE NUMBER OF BASE STATIONS
         IF (N.GT.0) NP=NP+1
50       CONTINUE
C
      IF (NNZ.GT.1) GO TO 80
C
C  REMOVE GROUPS WITH 0 OR 1 STATION
      IF (IG.EQ.NG) GO TO 70
      NN=IG+1
      DO 60 II=NN,NG
         NUM=NPG(II)
         IM=II-1
         NPG(IM)=NUM
         DO 60 I=1,NUM
            IGS(IM,I)=IGS(II,I)
60       CONTINUE
70    NG=NG-1
      IF (IG.GT.NG) GO TO 110
      GO TO 40
C
80    IF (NP.GT.1) GO TO 100
C
C  ONLY ONE POSITIVE STATION-MAKE ALL POSITIVE
      DO 90 I=1,NUM
         N=IGS(IG,I)
         IF (N.LT.0) IGS(IG,I)=-N
90       CONTINUE
      NP=NNZ
100   NPLUS(IG)=NP
      IG=IG+1
      IF (IG.GT.NG) GO TO 110
      GO TO 40
110   IF (NG.EQ.0) GO TO 370
C
C  GENERATE CONSISTENCY PLOTS
      CALL UPAGE (LP)
      WRITE (LP,410)
      WRITE (LP,420)
      WRITE (LP,430)
      WRITE (LP,440)
C
      DO 120 IG=1,NG
         WRITE (LP,450) IG
         WRITE (LP,460)
         NUM=NPG(IG)
         DO 120 I=1,NUM
            N=IGS(IG,I)
            NN=IABS(N)
            WRITE (LP,470) N,PXNAME(NN)
120      CONTINUE
C
C  COMPUTE BASE FOR EACH GROUP
      DO 150 IG=1,NG
         NUM=NPG(IG)
         FNP=NPLUS(IG)
         FNP=1.0/FNP
         BS=0.0
         DO 140 M=1,NMO
            BMO=0.0
            DO 130 I=1,NUM
               N=IGS(IG,I)
               IF (N.LT.0) GO TO 130
C           CALCULATE GROUP BASE MONTHLY PE
               BMO=BMO+APPE(N,M)*FNP
130            CONTINUE
C        CALCULATE GROUP BASE ACCUMULATED MONTHLY PE
            BS=BS+BMO
            BASE(IG,M)=BS
140         CONTINUE
150      CONTINUE
C
C  SET-UP PLOTS
      DO 360 IG=1,NG
         NUM=NPG(IG)
         FNP=NPLUS(IG)
         NPLOTS=(NUM-1)/5+1
         DO 350 IP=1,NPLOTS
            II=(IP-1)*5+1
            IL=IP*5
            IF (IL.GT.NUM) IL=NUM
            NN=IL-II+1
            DO 160 I=II,IL
               J=I-II+1
               N=IGS(IG,I)
               IGN(J)=IABS(N)
160            CONTINUE
            IPASS=1
170         CALL UPAGE (LP)
            WRITE (LP,480)
            WRITE (LP,490) (PXNAME(IGN(J)),J=1,NN)
            IF (NN.EQ.1) WRITE (LP,500)
            IF (NN.EQ.2) WRITE (LP,510)
            IF (NN.EQ.3) WRITE (LP,520)
            IF (NN.EQ.4) WRITE (LP,530)
            IF (NN.EQ.5) WRITE (LP,540)
            IF (LIDMA.GT.0) WRITE (LIDMA,495) (PXNAME(IGN(J)),J=1,NN)
CCC            IF (LIDMA.GT.0) IF (NN.EQ.2) WRITE (LIDMA,515)
C        COMPUTE PLOT SCALES AND PRINT ACCUMULATIONS
            DMAX=0.0
            DMIN=0.0
            BMAX=0.0
            BP=0.0
            DO 230 M=1,NMO
               BMO=BASE(IG,M)
               JYR=(M-1)/12
               MO=M-JYR*12
               MO=IMO+MO-1
               JYR=IYR+JYR
               IF (MO.LE.12) GO TO 180
                  MO=MO-12
                  JYR=JYR+1
180            IPRT=1
               IF (ICNCOP.EQ.1) GO TO 200
C            PERFORM CONSISTENCY ANALYSIS ON A SEASONAL BASIS
C            IF THE CURRENT MONTH IS NOT IN THE SEASON OF INTEREST,
C            IPRT IS SET TO 0.0
               IF (IPASS.EQ.2) GO TO 190
C            WINTER SEASON
               IF ((MO.GT.LMOW).AND.(MO.LT.IMOW)) IPRT=0
               GO TO 200
C            SUMMER SEASON
190            IF ((MO.LT.IMOS).OR.(MO.GT.LMOS)) IPRT=0
c
200            DO 220 I=II,IL
                  J=I-II+1
                  IF (M.GT.1) GO TO 210
C  INITIALIZE ARRAYS FOR ACCUMULATED STATION AND BASE MONTHLY PE, AND
C  FLAG FOR WHETHER MONTHLY PE AVAILABLE (0), ESTIMATED (1)
C  OR MISSING (2)
                  BSTA(J)=0.0
                  ACCUM(J)=0.0
                  IFLAG(J)=0
210               N=IGS(IG,I)
C              CALCULATE GROUP BASE PE FOR THE CURRENT MONTH
                  BS=BMO-BP
                  NA=IABS(N)
                  PXS=APPE(NA,M)
C              CALCULATE BASE WITH CURRENT STATION REMOVED
                  IF (N.GT.0) BS=(BS*FNP-PXS)/(FNP-1.0)
                  IF (IPRT.EQ.0) GO TO 220
C              ACCUMULATED STATION MONTHLY PE
                  ACCUM(J)=ACCUM(J)+PXS
C              ACCUMULATED BASE MONTHLY PE
                  BSTA(J)=BSTA(J)+BS
C              CALCULATE DEVIATION BETWEEN ACCUMULATED STATION
C              AND BASE MONTHLY PE
                  DEV=ACCUM(J)-BSTA(J)
C              STORE MAX AND MIN DEVIATIONS AND THE MAXIMUM ACCUMULATED
C              BASE PE FOR DETERMINING THE PLOT SCALE
                  IF (DEV.GT.DMAX) DMAX=DEV
                  IF (DEV.LT.DMIN) DMIN=DEV
                  IF (BSTA(J).GT.BMAX) BMAX=BSTA(J)
C  SET FLAG FOR AVAILABILITY OF MONTHLY PE DATA FOR I-DMA OUTPUT FILE
                  IFLAG(J) = PT_FLAG(NA,M)
220               CONTINUE
               BP=BMO
               IF (IPRT.EQ.0) GO TO 230
               WRITE (LP,550) MO,JYR,(ACCUM(J),BSTA(J),J=1,NN)
               IF (LIDMA.GT.0) WRITE (LIDMA,555)MO,JYR,(ACCUM(J),
     *            IFLAG(J),BSTA(J),J=1,NN)
230            CONTINUE
            CALL UPAGE (LP)
            WRITE (LP,560) IG
            WRITE (LP,580)
            DO 240 I=II,IL
               J=I-II+1
               N=IGS(IG,I)
               NA=IABS(N)
               WRITE (LP,570) N,J,PXNAME(NA)
240            CONTINUE
C         COMPUTE AND PRINT DEVIATION SCALE
            T25=BMAX*0.25
            T=DMAX-DMIN
            IF (T.LT.T25) T=T25
            KA=T*0.001+2.0
            K=T*0.1
            K=K+KA
            T=K*10.0
            DPL=T*0.01
            K=DMAX/DPL+1.0
            DMAX=K*DPL
            DMIN=DMAX-T
C           K IS LOCATION OF ZERO DEVIATION
            K=101-K
            WRITE (LP,590) DUNITS
            KA=0
            DO 250 IY=1,101,10
               KA=KA+1
               S(KA)=DMIN+DPL*(IY-1)
250            CONTINUE
            WRITE (LP,600) S
C           COMPUTE BASE PX SCALE
            KA=BMAX*0.1+1.0
            T=KA*10.0
            FINC=T*0.01
C        INITIALIZE GRID ARRAY
            DO 280 IX=1,101
               DO 260 IY=2,100
                  ORD(IX,IY)=BLANK
260               CONTINUE
               DO 270 IY=1,101,10
                  ORD(IX,IY)=DOT
270               CONTINUE
               ORD(IX,K)=ASTER
280            CONTINUE
C        FILL IN GRID ARRAY
            BP=0.0
            DO 330 M=1,NMO
               BMO=BASE(IG,M)
               JYR=(M-1)/12
               MO=M-JYR*12
               MO=IMO+MO-1
               IF (MO.GT.12)MO=MO-12
               IPRT=1
               IF (ICNCOP.EQ.1) GO TO 300
C            PERFORM CONSISTENCY ANALYSIS ON A SEASONAL BASIS
               IF (IPASS.EQ.2) GO TO 290
C            WINTER SEASON
               IF ((MO.GT.LMOW).AND.(MO.LT.IMOW)) IPRT=0
               GO TO 300
C            SUMMER SEASON
290            IF ((MO.LT.IMOS).OR.(MO.GT.LMOS)) IPRT=0
300            CONTINUE
               DO 320 I=II,IL
                  J=I-II+1
                  IF (M.GT.1) GO TO 310
                  BSTA(J)=0.0
                  ACCUM(J)=0.0
310               N=IGS(IG,I)
C               CALCULATE GROUP BASE PE FOR THE CURRENT MONTH
                  BS=BMO-BP
                  NA=IABS(N)
                  PXS=APPE(NA,M)
C               CALCULATE BASE PE WITH THE CURRENT STATION REMOVED
                  IF (N.GT.0) BS=(BS*FNP-PXS)/(FNP-1.0)
                  IF (IPRT.EQ.0) GO TO 320
C               CALCULATE ACCUMULATED STATION MONTHLY PE
                  ACCUM(J)=ACCUM(J)+PXS
C               CALCULATE ACCUMULATED BASE MONTHLY PE
                  BSTA(J)=BSTA(J)+BS
C               CALCULATE DEVIATION BETWEEN ACCUMULATED STATION AND BASE
C               MONTHLY PE
                  DEV=ACCUM(J)-BSTA(J)
                  IY=(DPL*0.5+DEV)/DPL
                  IY=IY+K
                  IF (DEV.LT.-0.5*DPL) IY=IY-1
                  IX=BSTA(J)/FINC+1.5
                  ORD(IX,IY)=SYM(J)
320               CONTINUE
C            SET BP EQUAL TO CURRENT MONTHS ACCUMULATED GROUP BASE PE
               BP=BMO
330            CONTINUE
C           PRINT GRID ARRAY
            DO 340 IX=1,101
               FIX=IX-1
               BS=FIX*FINC
               WRITE (LP,610) BS,(ORD(IX,IY),IY=1,101)
340            CONTINUE
            IF (ICNCOP.EQ.1) GO TO 350
            IF (IPASS.EQ.2) GO TO 350
            IPASS=2
            GO TO 170
350         CONTINUE
360      CONTINUE
C
      IF (LIDMA.GT.0) THEN
C     CLOSE IDMA OUTPUT FILE
         CALL UPCLOS (LIDMA,' ',ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,385) 'UPCLOS',ISTAT
            CALL UWARN (LP,0,-1)
            ENDIF
         ENDIF
C
370   RETURN
C
385   FORMAT ('0*** WARNING - IN MEPCON - STATUS FROM ROUTINE ',A,
     *   ' IS ',I2,'.')
380   FORMAT (16I5)
390   FORMAT ('0*** WARNING - ',
     *   'ICNCOP IS ',I4,' INDICATING NO CONSISTENCY CHECK IS ',
     *   'DESIRED, BUT NG IS ',I4,' AND NPG(1) IS ',I4 /
     *   ' SUGGESTING THAT A PLOT WAS WANTED.')
400   FORMAT (' ICNCOP WAS ',I4,' A CONSISTENCY PLOT WAS NOT ',
     *   'REQUESTED.')
410   FORMAT (///// 34X,'EVAPORATION CONSISTENCY CHECK')
420   FORMAT ('0STATIONS WITH POSITIVE NUMBERS CONSTITUTE THE ',
     *   'GROUP BASE AND ARE PLOTTED AGAINST THE OTHER STATIONS IN ',
     *   'THE GROUP BASE')
430   FORMAT ('0STATIONS WITH NEGATIVE RUN NUMBERS ARE PLOTTED ',
     *   'AGAINST THE GROUP BASE')
440   FORMAT ('0')
450   FORMAT ('0',10X,'STATIONS IN GROUP ',I2)
460   FORMAT ('0','STA. RUN NO.',8X,'STATION NAME')
470   FORMAT (' ',5X,I3,12X,A)
480   FORMAT ('0MONTHLY LISTING OF ACCUMULATED EVAPORATION ',
     *   'AND BASE EVAPORATION')
490   FORMAT (' ',16X,5(3X,A))
495   FORMAT (' ',15X,5(6X,A))
500   FORMAT (' MO/YEAR',3X,1(6X,'ACC. PE',3X,'BASE PE'))
510   FORMAT (' MO/YEAR',3X,2(6X,'ACC. PE',3X,'BASE PE'))
520   FORMAT (' MO/YEAR',3X,3(6X,'ACC. PE',3X,'BASE PE'))
530   FORMAT (' MO/YEAR',3X,4(6X,'ACC. PE',3X,'BASE PE'))
540   FORMAT (' MO/YEAR',3X,5(6X,'ACC. PE',3X,'BASE PE'))
515   format (' mo/year',3x,2(6x,'acc. pe',1x,'flg',2x,'base pe'))
550   FORMAT (' ',I2,'/',I4,3X,5(3X,2F10.2))
555   format (' ',i2,'/',i4,3x,5(3x,f10.2,i3,f10.2))
560   FORMAT ('0DEVIATION OF ACCUMULATED EVAPORATION FROM THE GROUP ',
     *   'BASE - GROUP=',I1)
570   FORMAT (' ',9X,I3,19X,I2,12X,A)
580   FORMAT ('0',5X,'STA. RUN NO.',8X,'STA. PLOT NO.',7X,
     *   'STATION NAME')
590   FORMAT ('0',10X,'ASTERISKS INDICATE ZERO DEVIATION',15X,
     *   'UNITS ARE ',A4)
600   FORMAT ('0BASE PX',11F10.1)
610   FORMAT (' ',F10.1,5X,101A1)
C
      END
