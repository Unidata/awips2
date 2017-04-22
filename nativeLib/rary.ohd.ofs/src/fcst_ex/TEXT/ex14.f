C MEMBER ENGL21
C  (from old member FCENGL21)
C
C @PROCESS LVL(77)
C.......................................................................
      SUBROUTINE EX14(PADJ,CADJ,QIN,QME,SQIN,QINE,QS,MISS,SQME,QI)
C                             LAST UPDATE: 10/12/95.09:15:40 BY $WC30KH
C
C
C     SUBROUTINE EX14 EXECUTES THE ADJUST OPERATION, WHICH ADJUSTS
C     SIMULATED INSTANTANEOUS DISCHARGE TO AGREE AS CLOSELY AS
C     POSSIBLE WITH OBSERVED INSTANTANEOUS DISCHARGE AND/OR WITH
C     OBSERVED MEAN DAILY DISCHARGE VOLUMES BEFORE ROUTING
C     DOWNSTREAM TO OTHER FLOWPOINTS.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE    JANUARY 1980
C
C     Modified by Scott Townsend RTi July 2003
C         Added code to fix a bug which kept an esp verification run
C         from completing since expected carryover values were not
C         written to the carryover file which was needed by the ESP
C         historic traces run.
C.......................................................................
C     VARIABLES IN ARGUMENT LIST:
C             PADJ   -PARAMETER ARRAY
C             CADJ   -CARRYOVER ARRAY
C             QIN    -OBSERVED INSTANTANEOUS DISCHARGE TIME SERIES
C             QME    -OBSERVED MEAN DAILY DISCHARGE TIME SERIES
C             SQIN   -SIMULATED INSTANTANEOUS DISCHARGE TIME SERIES
C             QINE   -ADJUSTED DISCHARGE TIME SERIES
C             QS     -WORKING DISCHARGE ARRAY
C             MISS   -WORKING ARRAY FOR MISSING DATA
C             SQME   -WORKING ARRAY FOR COMPUTED MEAN DAILYS
C             QI     -WORKING ARRAY FOR INST. OBS DATA
C.......................................................................
C
      DIMENSION PADJ(*),CADJ(*),QIN(*),QME(*),SQIN(*),QS(*),
     1 MISS(*),SQME(*),QINE(*),QI(*),E14(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fprog'
      INCLUDE 'common/esprun'
      INCLUDE 'common/egentr'
      INCLUDE 'common/eadjq'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex14.f,v $
     . $',                                                             '
     .$Id: ex14.f,v 1.6 2003/08/12 13:05:32 hank Exp $
     . $' /
C    ===================================================================
C
C
      DATA E14/4HEX14,4H    /
C
C     CHECK DEBUG REQUEST AND TRACE LEVEL
      CALL FPRBUG(E14,1,14,IBUG)
C     IF IBUG=1, PRINT DEBUG INFORMATION
C.......................................................................
C     RETRIEVE PARAMETERS AND CARRYOVER FROM PADJ AND CADJ ARRAYS.
      FACT=5.
      RDIFF=2.0
      IOI=PADJ(7)
      IOM=PADJ(8)
      ITSQ=PADJ(12)
      NSTEPS=PADJ(17)
      NCO=PADJ(18)
      MM=28
      NPARM=24
      IF(IOM.EQ.0) GO TO 10
      NPARM=NPARM+4
      TOL=PADJ(25)
      MM=32
 10   IF(IOI.EQ.0)GO TO 12
      NPARM=NPARM+5
      ITQI=PADJ(MM)
      IOPT=PADJ(MM+1)
 12   CONTINUE
      MC=24/ITSQ+1
      DQI=CADJ(MC+1)
      NBI=CADJ(MC+2)
      QBI=CADJ(MC+3)
      IF (NBI.LT.NSTEPS) GO TO 20
      DQI=0.0
      NBI=0
      QBI=0.0
C.......................................................................
C     SET POINTERS FOR TIME SERIES DATA ARRAYS
 20   IDT=ITSQ
      IK=1
 25   IF(IDT.GT.IHR)GO TO 30
      IB=(IDA-IDADAT)*24/IDT+IHR/IDT
      IE=(LDA-IDADAT)*24/IDT+LHR/IDT
      GO TO 35
 30   IB=(IDA-IDADAT)*24/IDT+(IHR-1)/IDT+1
      IE=(LDA-IDADAT)*24/IDT+(LHR-1)/IDT+1
 35   CONTINUE
      GO TO (40,45,55),IK
 40   IBSQ=IB
      IESQ=IE
      IF(IOM.EQ.0) GO TO 50
      IDT=24
      IK=2
      GO TO 25
 45   IBQM=IB
      IEQM=IE
 50   IF(IOI.EQ.0)GO TO 60
      IF(ITSQ.GE.ITQI) IDT=ITQI
      IK=3
      GO TO 25
 55   IBQI=IB
 60   CONTINUE
C.......................................................................
C      INITIALIZE QS AND QINE ARRAYS
      DO 62 I=IBSQ,IESQ
      QINE(I)=SQIN(I)
 62   QS(I)=SQIN(I)

C     JAY'S ESP STUFF
cew  when saving carryover for the esp verification 
cew  you need to save some carryover as a place holder.
cew  I commented out these lines to permit the system
cew  to run through the code and that created a problem
cew  with the ESP traces being adjusted to the obs values.
cew  NEW solution, let ESP go through the adjust code if
cew  it is iepass=2 (adjust run) as it is, else if igen=0
cew  skip operation as it does now, if igen=1 and 
cew  then jump to save CO section and then skip section
cew  where data is written to the QINE TS.

chh  We also want to use the operation if it is a CS run
chh  in which iepass == 3.
cmgm and the ESPADJQ Technique iespadjq == 1. 

cew  not an esp run
      IF(MAINUM.NE.2)GO TO 6000  

cew  ESP run and COSAVE also see second if below
chh  This line was changed to add the .and. iepass.ne.3 to allow
chh    ADJ-Q to run for CS runs.
	if(igen.eq.1 .and. iepass.ne.2 .and. iepass.ne.3)  go to 400  

CRTi Add the following line so that ESP verification runs will
CRTi still write a carryover line as a placeholder. If this is not
CRTi done then the carryover file will be missing a section of data
CRTi that is expected thus causing the generation of the ESP traces
CRTi for the historic simulation to fail while reading the carryover.
        IF(NCSTOR.GE.1 .AND. IEPASS.NE.2 .AND. IEPASS.NE.3)  GO TO 400  
       
cew  Regular esp (not adjust)
chh  A similar change is made here.
        IF(IEPASS.NE.2 .and. iepass.ne.3) GO TO 460 
        
cmgm If ESPADJQ Technique is off skip ADJ-Q for CS runs 
        IF(IEPASS.eq.3 .and. iespadjq.eq.0) GO TO 460

 6000 CONTINUE
C.......................................................................
C     PUT INSTANT. OBS T.S. DATA INTO QI WORKING ARRAY WITH SAME TIME
C     INTERVAL AS SQIN T.S.
      IF(IOI.EQ.0)GO TO 600
      IF(ITQI.NE.ITSQ)GO TO 615
cmgm r22 If Conditional Simulation (iepass=3) then set the observed 
c    streamflow to missing (otherwise ADJUST-Q tries to adjust to the 
c    historical streamflow). 
      DO 610 I=IBSQ,IESQ
      IF(iepass.eq.3)THEN
      QI(I)=-999.0
      ELSE
      QI(I)=QIN(I)
      ENDIF
cmgm end
 610  CONTINUE
      GO TO 600
 615  J=0
      DO 620 I=IBSQ,IESQ
      DT=ITQI
      RORD=(I*ITSQ)/DT
      JORD=(I*ITSQ)/ITQI
      REM=RORD-JORD
      QI(I)=-999.
      IF(ABS(REM).GT..001)GO TO 620
cmgm r22 If Conditional Simulation (iepass=3) then set the observed 
c    streamflow to missing (otherwise ADJUST-Q tries to adjust to the 
c    historical streamflow). 
      IF(IEPASS.EQ.3)THEN
      QI(I)=-999.0
      ELSE
      QI(I)=QIN(IBQI+J)
      ENDIF
cmgm end
      J=J+1
 620  CONTINUE
 600  CONTINUE
C.......................................................................
C     IF IBUG=1, PRINT DEBUG INFORMATION
      IF(IBUG.EQ.0)GO TO 64
      WRITE(IODBUG,9000)
 9000 FORMAT(1H0,41HCONTENTS OF THE PADJ ARRAY--FORMAT=15F8.2)
      WRITE(IODBUG,9010) (PADJ(I),I=1,NPARM)
 9010 FORMAT(1H0,7X,15F8.2)
      WRITE(IODBUG,9015)
 9015 FORMAT(1H0,41HCONTENTS OF THE CADJ ARRAY--FORMAT=15F8.2)
      WRITE(IODBUG,9020) (CADJ(I),I=1,NCO)
 9020 FORMAT(1H0,7X,15F8.2)
      WRITE(IODBUG,902) (SQIN(I),I=IBSQ,IESQ)
 902  FORMAT(1H0,19HSIMULATED DISCHARGE/(T1,8F10.3))
C.......................................................................
C     DETERMINE WHICH TYPE OF ADJUSTMENT TO USE BASED ON TYPE OF
C     OBSERVED DATA AVAILABLE.
 64   LOB=0
      IFIRST=0
      ISKIP=0
      KASE=1
      IF((IOM.EQ.1).AND.(IOI.EQ.1)) KASE=3
      IF((IOM.EQ.0).AND.(IOI.EQ.1)) KASE=2
      GO TO (65,200,300), KASE
C.......................................................................
C.......................................................................
C
C     OBSERVED MEAN DAILY DISCHARGE(QME) IS AVAILABLE. COMPUTE
C     SIMULATED MEAN DAILY DISCHARGE(SQME) FOR DAYS THAT HAVE A
C     QME VALUE. ADJUST SIMULATED VALUES BY RATIO OF QME TO SQME.
C
 65   CONTINUE
C
C  SET MISSING VALUE ARRAY. VALUE IS 'MISSING' EITHER IF IT EQUALS
C  -999.0 OR IF IGNORETS MOD IS IN EFFECT FOR THE PERIOD IN QUESTION
C
      IMTYPE = 2
      DO 70 I=IBQM,IEQM
      JULI = I*IDT + (IDADAT-1)*24
 70   MISS(I)=MAX0(IFMSNG(QME(I)),IFGNOR(IMTYPE,JULI,0))
      CALL FAJMDQ(QS,MISS,SQME,QME,IBQM,IEQM,IBSQ,IESQ,TOL,CADJ,
     1LASTOB,ITSQ,IHR,LHR)
C     IF IBUG=1, PRINT DEBUG INFORMATION
      IF(IBUG.EQ.0)GO TO 72
      WRITE(IODBUG,904) (QME(I),I=IBQM,IEQM)
 904  FORMAT(1H0,19HOBSERVED MDQ VALUES/(8F10.3))
      WRITE(IODBUG,900) (QS(I),I=IBSQ,IESQ)
 900  FORMAT(1H0,24HDISCH. AFTER MDQ ADJUST./(8F10.3))
 72   CONTINUE
      GO TO 400
C.......................................................................
C.......................................................................
C
C     OBSERVED INSTANTANEOUS DISCHARGE (QIN) DATA IS AVAILABLE.
C     IF OBS LESS THAN NSTEPS APART, INTERPOLATE THE RATIOS
C     (DIFFERENCES) AT THE OBSERVED ORDINATES TO OBTAIN THE
C     ADJUSTMENT FACTORS FOR THE INTERMEDIATE ORDINATES.IF
C     GREATER THAN NSTEPS APART, BLEND FORWARDS AND BACKWARDS
C     BETWEEN THE TWO OBS.
C
 200  CONTINUE
      IITYPE = 1
      DO 290 I=IBSQ,IESQ
      JULI = I*IDT + (IDA-1)*24
C
C  VALUE IS MISSING IF EQUAL TO -999. OR IGNORETS MOD IS IN EFFECT
C  FOR THIS PERIOD.
C
      MS=MAX0(IFMSNG(QI(I)),IFGNOR(IITYPE,JULI,0))
      IF(MS.GT.0)GO TO 290
      IRAT=0
      IF(IFIRST.GT.0)GO TO 250
      IFIRST=I
      LASTI=I
      IB=I
      IE=I
C.......................................................................
C     CHECK DISTANCE BETWEEN LAST OBSERVATION OF PREVIOUS
C     RUN AND FIRST OBS OF CURRENT RUN. IF DISTTANCE LESS
C     THAN NSTEPS, INTERPOLATE THE ADJUSTMENT FACTORS. IF
C     GREATER, BLEND BETWEEN THE TWO OBS.
C
      YD=QI(IFIRST)-QS(IFIRST)
      IF(IOPT.EQ.1)GO TO 210
C      IF(QS(IFIRST).LT..001)QS(IFIRST)=.01
      YR=FACT
      IF(QS(IFIRST).GE..001) YR=QI(IFIRST)/QS(IFIRST)
 210  IF(IFIRST.EQ.IBSQ)GO TO 285
      IF((NBI.EQ.0).AND.(ABS(DQI).LT..001))GO TO 228
      IDIST=NBI+IFIRST-IBSQ+1
      IF(IDIST.GE.NSTEPS)GO TO 225
C
C     INTERPOLATE
      XDOLD=DQI
      IF(IOPT.EQ.1)GO TO 215
      IF(YR.GE.FACT) IRAT=1
      DIV=QBI-DQI
      IF(DIV.LT..001)DIV=.01
      XROLD=QBI/DIV
      IF(XROLD.GE.FACT) IRAT=1
      IF(ABS(YR-XROLD).GT.RDIFF) IRAT=1
 215  IF(IRAT.EQ.1)GO TO 220
      IF(IOPT.EQ.1)GO TO 220
C
C     USE RATIOS
      DIFF=(YR-XROLD)/IDIST
      XR=XROLD+(NBI+1)*DIFF
      IB=IBSQ
      IEND=IE-1
      GO TO 251
C
C     USE DIFFERENCES
 220  DIFF=(YD-XDOLD)/IDIST
      XD=XDOLD+(NBI+1)*DIFF
      IB=IBSQ
      IEND=IE-1
      IZERO=0
      ISKIP=1
      GO TO 262
C
C     BLEND FORWARDS FROM LAST OBS. OF PREVIOUS RUN
 225  NBZ=NBI
      CALL FBLEND(QS,DQI,NBZ,NSTEPS,IBSQ,IESQ)
C     BLEND BACKWARDS FROM FIRST OBS. OF CURRENTT RUN
 228  NBLND=0
      IORD=I-NSTEPS+1
      IF(IORD.LT.IBSQ)IORD=IBSQ
      NORD=I-1
C     REVERSE ORDINATES IN QS ARRAY
      DO 230 K=IORD,NORD
      J=K-IORD
 230  QINE(K)=QS(NORD-J)
      CALL FBLEND(QINE,YD,NBLND,NSTEPS,IORD,NORD)
C     RESTORE BLENDED ORDINATES TO PROPER SEQUENCE
      DO 235 K=IORD,NORD
      J=K-IORD
 235  QS(NORD-J)=QINE(K)
      GO TO 285
C     END OF COMPUTATIONS PRIOR TO THE FIRST OBS.
C     FROM NOW ON, SKIP TO 250 FOR ADJUSTMENTS BETWEEN TWO OBS.
C.......................................................................
C
C     IS DISTANCE BETWEEN OBS. TOO LARGE FOR INTERPOLATING?
C
 250  LASTI=I
      IE=I
      YD=QI(I)-SQIN(I)
      IF(IOPT.EQ.1)GO TO 249
C      IF(QS(I).LT..001)QS(I)=.01
      YR=FACT
      IF(QS(I).GE..001) YR=QI(I)/QS(I)
 249  IF((IE-IB).GE.NSTEPS)GO TO 270
C.......................................................................
C     NO--INTERPOLATE
      IF(IOPT.EQ.1)GO TO 260
      IF(YR.GE.FACT) IRAT=1
      IF(XR.GE.FACT) IRAT=1
      IF(ABS(YR-XR).GT.RDIFF) IRAT=1
      IF(IRAT.EQ.1)GO TO 260
C     INTERPOLATE BETWEEN RATIOS
      IZERO=0
      DIFF=(YR-XR)/(IE-IB)
      IEND=IE-1
 251  DO 255 J=IB,IEND
      Z=XR+(J-IB)*DIFF
      IF(QS(J).GT..001)GO TO 252
      IF(J.EQ.IB)GO TO 252
      IZERO=1
      GO TO 255
 252  QS(J)=QS(J)*Z
 255  CONTINUE
C
C     IF ANY SIM. Q VALUES WERE 0 AND THUS COULD NOT BE ADJUSTED
C     BY THE RATIOS, ESTIMATE ADJUSTED VALUES BY INTERPOLATING
C     BETWEEN ADJUSTED NON-ZERO VALUES
C
      IF(IZERO.EQ.0)GO TO 285
      INT=1
      J=IB+1
      A=QS(IB)
 2000 IF(QS(J).GT..001)GO TO 2010
      INT=INT+1
      J=J+1
      IF(J.GE.IE)GO TO 2020
      GO TO 2000
 2010 IF(INT.GT.1)GO TO 2020
      IF(J.GE.IEND)GO TO 285
      A=QS(J)
      J=J+1
      GO TO 2000
 2020 DF=(QS(J)-A)/INT
      IF(J.EQ.IE)DF=((QS(J)*YR)-A)/INT
      L1=J-INT+1
      L2=J-1
      DO 2025 L=L1,L2
 2025 QS(L)=A+(L-L1+1)*DF
      IF(J.GE.IEND)GO TO 285
      A=QS(J)
      J=J+1
      INT=1
      GO TO 2000
C
C     INTERPOLATE BETWEEN DIFFERENCES
 260  DIFF=(YD-XD)/(IE-IB)
      IEND=IE-1
 262  DO 265 J=IB,IEND
      Z=XD+(J-IB)*DIFF
      QS(J)=QS(J)+Z
      IF(ISKIP.EQ.1)GO TO 264
      IF(J.EQ.IB)QS(J)=QI(J)
 264  IF(QS(J).LT..001)QS(J)=0.
 265  CONTINUE
      ISKIP=0
      GO TO 285
C.......................................................................
C     YES--DO BLENDING
C
C     FORWARD BLEND FROM THE FIRST OF THE TWO OBS
 270  NBLND=0
      QS(IB)=QI(IB)
      IORD=IB+1
      CALL FBLEND(QS,XD,NBLND,NSTEPS,IORD,IESQ)
C     REVERSE ORDINATES FOR BACKWARD BLEND
      IORD=IE-NSTEPS+1
      NORD=IE-1
      NBLND=0
      DO 275 K=IORD,NORD
      J=K-IORD
 275  QINE(K)=QS(NORD-J)
      CALL FBLEND(QINE,YD,NBLND,NSTEPS,IORD,NORD)
C     RESTORE BLENDED ORDINATES TO PROPER SEQUENCE
      DO 280 K=IORD,NORD
      J=K-IORD
 280  QS(NORD-J)=QINE(K)
C.......................................................................
C
 285  IF(IOPT.EQ.0)XR=YR
      XD=YD
      IB=IE
 290  CONTINUE
C.......................................................................
C
      IF(IFIRST.EQ.0)GO TO 292
      QS(IE)=QI(IE)
      GO TO 295
C     FINISH BLEND OF PREVIOUS RUN EVEN IF IFIRST=0
 292  IF((NBI.EQ.0).AND.(ABS(DQI).LT..001))GO TO 298
      NBZ=NBI
      CALL FBLEND(QS,DQI,NBZ,NSTEPS,IBSQ,IESQ)
      GO TO 298
C     BLEND AFTER LAST OBS OF CURRENT RUN
 295  IF(IE.EQ.IESQ)GO TO 298
      DELQ=QI(IE)-SQIN(IE)
      NBLND=0
      ISIM=IE+1
      CALL FBLEND(QS,DELQ,NBLND,NSTEPS,ISIM,IESQ)
C
C     IF IBUG=1, PRINT DEBUG INFORMATION
C
 298  IF(IBUG.EQ.0)GO TO 299
      WRITE(IODBUG,905) (QIN(I),I=IBSQ,IESQ)
 905  FORMAT(1H0,25HINST. DISCH. OBSERVATIONS/(8F10.3))
      WRITE(IODBUG,906) (QS(I),I=IBSQ,IESQ)
 906  FORMAT(1H0,26HDISCH. AFTER INST. ADJUST./(8F10.3))
      WRITE(IODBUG,9015)
      WRITE(IODBUG,9020) (CADJ(I),I=1,NCO)
C
 299  CONTINUE
      IF(KASE.EQ.3)GO TO 310
      GO TO 400
C.......................................................................
C.......................................................................
C
C      BOTH OBSERVED MDQ'S AND INSTANTANEOUS DISCHARGE DATA ARE
C      AVAILABLE. FIRST MAKE ADJUSTMENTS USING INSTANTANEOUS DATA.
C      THEN COMPUTE MDQ'S USING ADJUSTED DISCHARGE AND PROCEED
C      WITH THE MEAN DAILY ADJUSTMENTS.
C
 300  GO TO 200
 310  GO TO 65
C.......................................................................
C.......................................................................
C
C
C     UPDATE CARRYOVER IF REQUESTED
 400  IF(IFILLC.EQ.0) GO TO 1000
      ICALL=0
      IF(NCSTOR.NE.0)GO TO 407
 405  ICALL=1
      MDA=LDA
      MHR=LHR
      GO TO 408
 407  I=1
 406  MDA=ICDAY(I)
      MHR=ICHOUR(I)
cew include this check for the ESP runs to be sure the carryover day is within run 
cew  period
      if(icday(i) .lt. ida) goto 1000
      if(icday(i) .gt. lda) goto 1000
      if(icday(i) .eq. ida .and. ichour(i) .lt. ihr) goto 1000
      if(icday(i) .eq. lda .and. ichour(i) .gt. lhr) goto 1000

 408  IF(ITSQ.GT.MHR) GO TO 410
      L=(MDA-IDADAT)*24/ITSQ+MHR/ITSQ
      GO TO 415
 410  L=(MDA-IDADAT)*24/ITSQ+(MHR-1)/ITSQ +1
 415  NN=MHR/ITSQ+1
      N1=NN+1
      N2=24/ITSQ+1
      IF(N1.LE.N2)GO TO 412
      N1=2
      CADJ(1)=SQIN(L)
      GO TO 421
 412  DO 420 J=1,NN
      K=L-(NN-J)
      IF(K.LT.IBSQ)GO TO 418
      CADJ(J)=SQIN(K)
      GO TO 420
 418  IF(IHR.EQ.ITSQ)CADJ(J)=CADJ(N2)
 420  CONTINUE
 421  DO 422 J=N1,N2
 422  CADJ(J)=0.
      IF(IOI.EQ.0)GO TO 430
      LOB=0
      KL=L-NSTEPS
      IF(KL.LT.IBSQ)KL=IBSQ
      DO 500 IXX=KL,L
      IF(QI(IXX).LT.-.001)GO TO 500
      IF(IXX.GT.LOB)LOB=IXX
 500  CONTINUE
      IF(LOB.EQ.0)GO TO 428
      NEX=L-LOB
      IF(NEX.EQ.NSTEPS)GO TO 430
      CADJ(N2+1)=QS(LOB)-SQIN(LOB)
      CADJ(N2+2)=NEX+.01
      CADJ(N2+3)=QS(LOB)
      GO TO 432
 428  IF((NBI.EQ.0).AND.(ABS(DQI).LT..001))GO TO 430
      NDIST=NBI+L-IBSQ+1
      IF(NDIST.GE.NSTEPS)GO TO 430
      CADJ(N2+1)=DQI
      CADJ(N2+2)=NDIST+.01
      CADJ(N2+3)=QBI
      GO TO 432
 430  CADJ(N2+1)=0.
      CADJ(N2+2)=0+.01
      CADJ(N2+3)=0.
 432  CONTINUE
C     SAVE CARRYOVER IF REQUESTED
      IF(ICALL.EQ.1)GO TO 1000
      IF(NCSTOR.EQ.0)GO TO 1000
C     WRITE CARRYOVER ONTO FILE
      CALL FCWTCO(MDA,MHR,CADJ,NCO)
      IF(I.EQ.NCSTOR)GO TO 405
      I=I+1
      GO TO 406
C.......................................................................
C.......................................................................
C
C     PLACE ADJUSTED VALUES IN QINE TIME SERIES
cew   ESP run and COSAVE see second if below
      if(igen.eq.1 .and. iepass .ne. 2)  go to 460
 1000 DO 450 I=IBSQ,IESQ
 450    QINE(I)=QS(I)

 460  CONTINUE
      RETURN
      END
