C MEMBER EX8
C  (from old member FCEX8)
C.......................................................................
      SUBROUTINE EX8(PLOSS,CLOSS,Q,PE)
C
C     SUBROUTINE EX8 EXECUTES THE LOSS OPERATION, WHICH
C     ADJUSTS INSTANTANEOUS FLOW HYDROGRAPHS FOR LOSSES
C     (OR GAINS) OF WATER DUE TO FLOW THROUGH THE CHANNEL
C     BOTTOM/BANKS AND EVAPORATION FROM THE WATER SURFACE.
C
C.......................................................................
C     PROGRAMMED BY KAY KROUSE   OCTOBER 1979
C.......................................................................
C     VARIABLES IN ARGUMENT LIST:
C                1) PLOSS    -ARRAY CONTAINING PARAMETERS,
C                             TIME SERIES IDENTIFIERS, OPTIONS,
C                             ETC. FOR LOSS OPERATION
C                2) Q        -DISCHARGE TIME SERIES
C                3) PE       -PE TIME SERIES, IF AVAILABLE
C
C.......................................................................
      INTEGER DA,YR,HR,SSWCH,ESWCH,PEDATA
      DIMENSION Q(1),PE(1),PLOSS(1),DIST(24),EPDIST(24),ND(12),CLOSS(1)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex8.f,v $
     . $',                                                             '
     .$Id: ex8.f,v 1.1 1995/09/17 18:57:24 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA DIST/8*0.0,0.02,0.05,0.10,0.16,0.20,0.18,0.14,0.09,0.05,0.01,
     16*0.0/
      DATA ND/31,28,31,30,31,30,31,31,30,31,30,31/
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
 900  FORMAT(1H0,14H** EX8 ENTERED)
C.......................................................................
C     DEBUG OUTPUT?  IF IBUG=1, PRINT DEBUG INFORMATION.
      IBUG=0
      IF(IDBALL.GT.0)IBUG=1
      IF(NDEBUG.EQ.0)GO TO 12
      DO 10 I=1,NDEBUG
      IF(IDEBUG(I).EQ.8)GO TO 11
   10 CONTINUE
      GO TO 12
   11 IBUG=1
   12 CONTINUE
C.......................................................................
C     CONVERT INITIAL AND FINAL DATES (INTERNAL CLOCK)
C     TO ACTUAL MONTH, DAY, YEAR AND HOUR.
C
      INTIME=IHR
      NO=0
      CALL MDYH1(IDA,IHR,MO,DA,YR,HR,NLSTZ,NO,ZONE)
      CALL MDYH1(LDA,LHR,JMO,JDA,JYR,JHR,NLSTZ,NO,ZONE)
      KMO=MO
      KDA=DA
      KYR=YR
      KHR=HR
C.......................................................................
C     LOCATE DISCHARGE DATA IN Q TIME SERIES
C
      IDT=PLOSS(10)
      IBEGIN=(IDA-IDADAT)*24/IDT+IHR/IDT
      IEND=(LDA-IDADAT)*24/IDT+LHR/IDT
C.......................................................................
C     DEBUG INFORMATION
      IF(IBUG.EQ.1) WRITE(IODBUG,910) (Q(I),I=IBEGIN,IEND)
  910 FORMAT(1H0,18HINITIAL DISCHARGES,(T30,10F10.1))
C.......................................................................
C     APPLY SSOUT LOSS PARAMETER TO DISCHARGE.  IF NO
C     CHANNEL BOTTOM LOSS OCCURS (SSOUT=0), GO TO
C     THE EVAPORATION LOSS COMPUTATIONS.
C
      WSAREA=PLOSS(11)
      MM=11
      IF(WSAREA.GT.0.0)GO TO 25
      SSOUT=PLOSS(MM+1)
      SSWCH=PLOSS(MM+2)
      MM=MM+2
      GO TO 30
   25 PEDATA=PLOSS(MM+1)
      MM=MM+1
      IF(PEDATA.EQ.1)MM=MM+5
      SSOUT=PLOSS(MM+1)
      SSWCH=PLOSS(MM+2)
      MM=MM+2
   30 IF(SSWCH.EQ.0)GO TO 75
C
C     IS SSOUT CONSTANT, A FIXED PERCENTAGE, VARIABLE OR A VARIABLE % ?
      IF((SSWCH.EQ.3).OR.(SSWCH.EQ.4))GO TO 55
      IF(SSWCH.EQ.2)GO TO 40
C
C     SSOUT IS CONSTANT.
C     NOTE**A NEGATIVE SSOUT INDICATES A GAIN INSTEAD OF A LOSS.
      DO 35 I=IBEGIN,IEND
      Q(I)=Q(I)-SSOUT
      IF(Q(I).LT.0.0) Q(I)=0.0
   35 CONTINUE
      GO TO 75
C
C     SSOUT IS A FIXED PERCENTAGE.
   40 DO 45 I=IBEGIN,IEND
      Q(I)=Q(I)-(SSOUT*Q(I))
      IF(Q(I).LT.0.0)Q(I)=0.0
   45 CONTINUE
      GO TO 75
C
C     SSOUT IS VARIABLE.  USE MID-MONTH VALUES AND
C     ADJUST FOR THE CURRENT DAY WITH THE DAILY INCREMENT
   55 DO 60 I=IBEGIN,IEND
      IDAY=KDA-16
      M=KMO-1
      IF(M.EQ.0)M=12
      MK=MM+KMO
      ML=MM+12+M
      IF(KDA.GE.16) ML=MM+12+KMO
      SS=(PLOSS(MK)+PLOSS(ML)*IDAY)
      IF(SSWCH.EQ.3) Q(I)=Q(I)-SS
      IF(SSWCH.EQ.4) Q(I)=Q(I)-(SS*Q(I))
      IF(Q(I).LT.0.0) Q(I)=0.0
C
C     INCREMENT TIMING
      KHR=KHR+IDT
      IF(KHR.LE.24)GO TO 60
      KHR=IDT
      KDA=KDA+1
      ID=ND(KMO)
      IF((((KYR/4)*4).EQ.KYR).AND.(KMO.EQ.2))ID=ID+1
      IF(KDA.LE.ID) GO TO 60
      KDA=1
      KMO=KMO+1
      IF(KMO.LE.12)GO TO 60
      KMO=1
      KYR=KYR+1
   60 CONTINUE
      MM=MM+24
C.......................................................................
C
C     DEBUG INFORMATION
   75 IF(IBUG.EQ.1) WRITE(IODBUG,920) (Q(I),I=IBEGIN,IEND)
  920 FORMAT(1H0,27HDISCHARGES AFTER SSOUT LOSS,(T30,10F10.1))
C.......................................................................
C
C     DETERMINE IF AN EVAPORATION LOSS IS TO BE
C     COMPUTED.  IF NOT (WSAREA=0.0), NO FURTHER
C     COMPUTATIONS ARE NECESSARY.
C
      IF(WSAREA.LE.0.0) GO TO 170
C
      K=IBEGIN
      IP=0
      KMO=MO
      KDA=DA
      KYR=YR
      KHR=HR
      LN=LHR/IDT
C     DETERMINE DAILY EVAP-DISTRIBUTION
      NINT=24/IDT
      ESWCH=PLOSS(MM+1)
      PEADJ=PLOSS(MM+2)
      MM=MM+2
      IF(ESWCH.GT.0)GO TO 80
C     UNIFORM DAILY EVAP-DISTRIBUTION
      V=1.0/NINT
      DO 78 I=1,NINT
      EPDIST(I)=V
   78 CONTINUE
      GO TO 86
C     DIURNAL DAILY EVAP-DISTRIBUTION
   80 DO 85 I=1,NINT
      I2=I*IDT
      I1=(I-1)*IDT+1
      EPDIST(I)=0.0
      DO 85 J=I1,I2
      EPDIST(I)=EPDIST(I)+DIST(J)
   85 CONTINUE
C
C     LOCATE PE DATA IN TIME SERIES, IF AVAILABLE
  86  IF(PEDATA.EQ.0) GO TO 90
      ITPE=PLOSS(16)
      IP=(IDA-IDADAT) + 1
      CPEADJ = CLOSS(1)*PEADJ
C
C     COMPUTE EVAP-DEMAND/PE-ADJ CURVE VALUES FOR CURRENT DAY
   90 IDAY=KDA-16
      M=KMO-1
      IF(M.EQ.0) M=12
      MK=KMO+MM
      ML=MM+12+M
      IF(KDA.GE.16) ML=MM+12+KMO
      E=PLOSS(MK)+PLOSS(ML)*IDAY
C
C     COMPUTE ACTUAL EVAP-DEMAND FOR PERIOD-(MM/IDT)
  105 KINT=((KHR-1)/IDT) +1
      IF(PEDATA.EQ.0)GO TO 110
C     PE DATA IS USED
      IF(LDA.GT.IDA)GO TO 106
      IF(LHR.EQ.24) GO TO 106
      ET=CPEADJ*E*EPDIST(KINT)
      GO TO 115
 106  ET=PE(IP)*PEADJ*E*EPDIST(KINT)
      GO TO 115
C     NO PE DATA
  110 ET=PEADJ*E*EPDIST(KINT)
C
C     CONVERT ET IN MM/IDT TO INSTANTANEOUS LOSS
C     RATE IN CMS.
C
 115  ET=(.27778/IDT)*ET*WSAREA
C.......................................................................
C
C     SUBTRACT EVAPORATION LOSS FROM DISCHARGE
      Q(K)=Q(K)-ET
      IF(Q(K).LT.0.0) Q(K)=0.0
C.......................................................................
C
C     INCREMENT TIMING
      IF(K.EQ.IEND) GO TO 150
      KHR=KHR+IDT
      K=K+1
      IF(KHR.LE.24) GO TO 120
      KHR=KHR-24
      KDA=KDA+1
      ID=ND(KMO)
      IF((((KYR/4)*4).EQ.KYR).AND.(KMO.EQ.2)) ID=ID+1
      IF(KDA.LE.ID) GO TO 120
      KDA=1
      KMO=KMO+1
      IF(KMO.LE.12) GO TO 120
      KMO=1
      KYR=KYR+1
 120  IF(PEDATA.EQ.0)GO TO 90
      INTIME=INTIME+IDT
      IF(INTIME.LE.24)GO TO 90
      INTIME=INTIME-24
      IP=IP+1
      IF(LHR.EQ.24) GO TO 90
      IF ((IEND-K).LT.LN) IP = IP-1
      GO TO 90
C.......................................................................
 150  IF(PEDATA.EQ.0)GO TO 170
C     UPDATE CARRYOVER IF REQUESTED
      IF(IFILLC.EQ.0)GO TO 170
      IF(NCSTOR.EQ.0)GO TO 165
      I=1
 160  MDA=ICDAY(I)
      MHR=ICHOUR(I)
      L=MHR/ITPE
      IP=(MDA-IDADAT)*24/ITPE+L
      IF(MDA.GT.IDA)GO TO 161
      IF(MHR.EQ.24)GO TO 161
      PECO=CLOSS(1)
      GO TO 162
 161  PECO=PE(IP)
 162  NCO=1
      CALL FCWTCO(MDA,MHR,PECO,NCO)
C     SAVE CARRYOVER IF REQUESTED
      IF(I.EQ.NCSTOR)GO TO 165
      I=I+1
      GO TO 160
 165  IP=(LDA-IDADAT)*24/ITPE+LHR/ITPE
      IF(LDA.GT.IDA)GO TO 166
      IF(LHR.EQ.24)GO TO 166
      GO TO 170
 166  CLOSS(1)=PE(IP)
C.......................................................................
 170  CONTINUE
C     DEBUG INFORMATION
      IF(IBUG.EQ.1) WRITE(IODBUG,930) (Q(I),I=IBEGIN,IEND)
  930 FORMAT(1H0,28HDISCH. AFTER SSOUT/EVAP LOSS,(T30,10F10.1))
      RETURN
      END
