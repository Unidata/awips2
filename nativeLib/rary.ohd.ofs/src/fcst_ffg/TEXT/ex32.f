C MEMBER EX32
C  (from old member FCEX32)
C VERSION 1.13
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/08/95.14:08:22 BY $WC20SV
C
C  PGM:  EX32 (PO,PS,CS,PR,CR,TASN,TARR,RSTS,FFG)
C
C   IN: PO     .... PARAMETRIC DATA FROM P ARRAY
C   IN: PS     .... PARAMETRIC DATA FOR SNOW OPERATION FROM P ARRAY
C   IN: CS     .... CARRYOVER DATA FOR SNOW OPERATION FROM C ARRAY
C   IN: PR     .... PARAMETRIC DATA FROM RAINFALL-RUNOFF OPERATION
C                     FROM P ARRAY
C   IN: CR     .... CARRYOVER DATA FOR RAINFALL-RUNOFF OPERATION
C                     FROM C ARRAY
C   IN: TASN   .... SNOW MODEL AIR TEMPERATURE ARRAY FROM D ARRAY
C   IN: TARR   .... RAINFALL-RUNOFF MODEL AIR TEMPERATURE ARRAY
C                     FROM D ARRAY
CEA IN: RSTS   .... RAIN-SNOW ELEVATION ARRAY FROM D ARRAY
C   IN: FFG    .... FLASH FLOOD GUIDANCE PARAMETER ARRAY (INTERNAL)
C  =====================================================================
C @PROCESS LVL(77)
C
      SUBROUTINE EX32 (PO,PS,CS,PR,CR,TASN,TARR,RSTS,FFG)
C
C.......................................................................
C  THIS ROUTINE EXECUTES THE FFG OPERATION
C
C.......................................................................
C  INITIALY WRITTEN BY
C        JANICE LEWIS, HYDROLOGIC RESEARCH LAB, OCT 1991
C
C    EXPANDED FOR USE WITH API RAINFALL-RUNOFF OPERATIONS (CONTINUOUS
C    AND EVENT).
C        TIM SWEENEY, HRL                  JAN 1995      VERSION 1.10
C
C  Corrected logic for air temp calculation in snow model when
c  additional temperature parameters not used, i.e. elevation of temp
c  data same as elevation of area.
c        Tim Sweeney, HRL                  Dec 1996      Version 1.11
C
C  CHANGED SNOW MODEL PORTION TO TAKE INTO ACCOUNT ADDITIONS THAT HAVE
C    BEEN MADE TO SNOW-17 RELATED TO SNOW DEPTH AND SNOWPACK
c    TEMPERATURE.  ALSO MADE SOME CORRECTIONS AND COMMENTS RELATED
C    TO THE SNOW MODEL.
C        Eric Anderson                     Sept 2006
C.......................................................................
C  PRINCIPLE VARIABLES...
C
C  FOR DEFINITION OF VARIABLES IN COMMON BLOCKS, SEE SECTION IX.3.3C
C  OF THE NWSRFS USER'S MANUAL.
C
C     CAESC
C     CR(1)       CURRENT CARRYOVER DATA FROM RAINFALL-RUNOFF MODEL
C     CS(1)       CURRENT CARRYOVER DATA FROM SNOW MODEL
C     CWE
C     DT
C     EPDIST
C     FFG         FLASH FLOOD GUIDANCE PARAMETER ARRAY
C     FFGDSC      FLASH FLOOD GUIDANCE AREA DESCRIPTION
C     IBUG        DEBUG OUTPUT SWITCH, 0 = OFF, 1 = ON
C     ICLOC       STARTING LOCATION OF SNOW MODEL INFO IN FFG ARRAY
C     IDBB        ID OF BASIN BOUNDARY
C     IDFFG       FLASH FLOOD GUIDANCE AREA IDENTIFIER
C     IDAY
C     IDEL
C     IDN
C     IDT
C     IDUR        DURATION FLAG, 0 = 1-, 3-, AND 6-HOUR DURATIONS ONLY,
C                                1 = 12-HOUR DURATION ALSO,
C                                2 = 12- AND 24-HOUR DURATIONS ALSO.
C     IFATAL      FATAL ERROR FLAG, 0 = NO FATAL ERRORS, 1 = ERRORS
C     IFFG        FLASH FLOOD GUIDANCE SWITCH: 0=NO FFG, 1=FFG ONLY,
C                    2=FFG AND REGULAR RUN
C     IFRZE
C     IFUT
C     IHOUR
C     IMN
C     IOUT
C     IPRINT
C     IPTR        STARTING LOCATION OF BASIN PARAMETERS IN PREPROCESSOR
C                   PARAMETRIC DATA BASE
C     IPTRNX      POINTER TO NEXT PARAMETER RECORD OF 'FFG' DATA TYPE
C     IRLOC       STARTING LOCATION OF RAINFALL-RUNOFF MODEL INFO IN FFG
C                   ARRAY
C     ISTAT       BASIN BOUNDARY STATUS CODE, 0 = SUCCESSFUL READ,
C                   2 = RECORD NOT FOUND, ELSE NOT 0 OR 2 UNABLE TO
C                   READ RECORD
C     ITP
C     ITYFFG      FLASH FLOOD GUIDANCE AREA TYPE (FFG)
C     IVOPT
C     ITPE
C     ITSC
C     ITWE
C     KHR         SPECIFIED HOUR FOR TODAY'S DATE, RANGE 1-24
C     KMO,KDA,KYR TODAY'S DATE (MONTH, DAY, YEAR, 2 DIGITS EACH)
C     LTA
C     MFFG        MAXIMUM SIZE OF FLASH FLOOD GUIDANCE PARAMETER ARRAY
C     NCORR       NUMBER OF RAINFALL-RUNOFF MODEL CARRYOVER VALUES
C     NCOSN       NUMBER OF SNOW MODEL CARRYOVER VALUES
CEA                 NOTE: THIS IS THE NUMBER OF SNOW MODEL CARRYOVER
CEA                  VALUES RETAINED IN THE 'FFG' PARAMETER ARRAY IN
CEA                  THE PPPDB - NOT THE NUMBER OF CARRYOVER VALUES
CEA                  ACTUALLY USED BY THE SNOW-17 OPERATION.
C     NDT         NUMBER OF TIME INCREMENTS NEEDED TO AVERAGE THE
C                   TEMPERATURE
CEA               NOTE: NDT IS ACTUALLY THE NUMBER OF PRECIPITATION
CEA                TIME INTERVALS PER TEMPERATURE TIME INTERVAL FOR
CEA                THE SNOW MODEL - IT IS ALWAYS EQUAL TO 1 FOR FFG.
C     NFILL       NUMBER OF FULL WORDS FILL IN FFG ARRAY
C     NOP         NUMBER OF THIS OPERATION (32)
C     NOPRR       NUMBER OF OPERATION ASSIGNED TO RAINFALL-RUNOFF
C                   OPERATION
C     NOPSN       NUMBER OF OPERATION ASSIGNED TO SNOW OPERATION
C     OPNARR2)    OPERATION NAME OF RAINFALL-RUNOFF MODEL
C     OPNASN(2)   OPERATION NAME OF SNOW MODEL
C     OPTYRR(2)   OPERATION TYPE OF RAINFALL-RUNOFF MODEL
C     OPTYSN(2)   OPERATION TYPE OF SNOW MODEL
C     PCOVER
C     PEADJ
C     PGM
C     PO(1)       INPUT PARAMETRIC DATA FROM P ARRAY
C     POSC
C     POWE
C     PPCTS
C     PPX         PRECIPITATION FOR THE CURRENT TIME
C     PR(1)       PARAMETRIC DATA FROM RAINFALL-RUNOFF MODEL
C     PS(1)       PARAMETRIC DATA FROM SNOW MODEL
C     PTA         AIR TEMPERATURE FOR THE CURRENT TIME
C     RO          RUNOFF CORRESPONDING TO PRECIPITATION PPX
C     SNAME(2)    ROUTINE NAME
C     TMPSN       TEMPERATURE FOR THE SNOW OPERATION
C     TSTADT      TIME INTERVAL OF AIR TEMPERATURE TIME SERIES FOR
C                   SNOW MODEL
C     TWE         SIMULATED WATER EQUIVALENT
C     VERS        VERSION NUMBER
C
C
C.......................................................................
C
      CHARACTER*4 SNAME(2),IDFFG(2),FFGDSC(5),ITYFFG
      CHARACTER*4 IDBB(2),OPNARR(2),OPTYRR(2),OPNASN(2),OPTYSN(2)
      INTEGER SAVOUT
C.......................................................................
C
C  COMMON BLOCKS
      INCLUDE 'common/ffgctl'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fsacpr'
      INCLUDE 'common/fsnw'
      INCLUDE 'common/fclfls'
      COMMON  /OUTCTL/IOUTYP
C.......................................................................
C
      DIMENSION PO(*),IRROP(6),PR(*),CR(*),PS(*),CS(*),TASN(*),TARR(*)
      DIMENSION IDANG(12),ITFFG(5),FFG(*),EPDIST(24),RSTS(*),PCIN(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/ex32.f,v $
     . $',                                                             '
     .$Id: ex32.f,v 1.6 2006/10/04 12:08:45 hsu Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C
      DATA SNAME/4hEX32,4h    /,NOP/32/
      DATA ITYFFG/4hFFG /
      DATA ITYPBB/4hBASN/
      DATA ITFFG/1,3,6,12,24/
      DATA IRROP/1,24,29,33,35,43/
      DATA IDANG/285,316,345,10,40,71,101,132,163,193,224,254/
      DATA PCIN/0.0,0.30,0.60,1.0/
C.......................................................................
C
C  CALL DEBUG CHECK ROUTINE
C  TRACE LEVEL=1, DEBUG FLAG=IBUG
C
      VERS=1.13
C
      CALL FPRBUG (SNAME,1,NOP,IBUG)
      IF(IBUG.GT.0) WRITE(IODBUG,7000) SNAME,VERS
 7000 FORMAT(/5X,2A4,' DEBUG OUTPUT.',5X,'VERSION:',F5.2)
C
C.......................................................................
      IF(IFFG.EQ.0) THEN
        IF(IBUG.GT.0) WRITE(IODBUG,7001)
 7001   FORMAT(/5X,'====== THE FLASH FLOOD OPTION HAS BEEN TURNED OFF',
     .             ' ======='/)
         GO TO 8000
      ENDIF
      IFATAL = 0
      MXITER = 20
C.......................................................................
C  CONVERSION FACTOR
      CONV = 25.4
C
C  SET FLAG TO COMPUTE MINIMUM THRESHOLD RUNOFF VALUE
      MTHRSH = 1
C.......................................................................
C  SELECT REQUIRED VARIABLES FROM PO ARRAY
C
      VERS = PO(1)
      CALL UMEMOV(PO(2),IDFFG,2)
      CALL UMEMOV(PO(4),FFGDSC,5)
      CALL UMEMOV(PO(9),OPTYRR,2)
      CALL UMEMOV(PO(11),OPNARR,2)
      CALL UMEMOV(PO(13),OPTYSN,2)
      CALL UMEMOV(PO(15),OPNASN,2)
      CALL UMEMOV(PO(17),IDBB,2)
      IDUR = PO(19)
      NOPSN = PO(20)
      NOPRR = PO(21)
C
C       THE FOLLOWING STATEMENT SHOULD REMAIN DISABLED UNTIL
C        THE REORDER PROGRAM IS CHANGED TO UPDATE THE FFG
C        PARAMETER ARRAY RECORD NUMBER
C
C     IPTR = PO(22)
C
      IPTR = 0
      NFILL = PO(23)
      MFFG = NFILL
C
C  LOCATION OF MINIMUM AND MAXIMUM THRESHOLD RUNOFF VALUES IN PO ARRAY
      LMMRO = PO(24)
C
C  SET MINIMUM AND MAXIMUM THRESHOLD RUNOFF VALUES
      IF(LMMRO.GT.0) THEN
        ROMIN = PO(LMMRO)
        ROMAX = PO(LMMRO+1)
        N = 31
      ELSE
        ROMIN = 0.1
        ROMAX = 2.5
        N = 24
      ENDIF
C
C-----------------------------------------------------------------------
C  PRINT THE CONTENTS OF THE PO ARRAY
C
      IF(IBUG.EQ.0) GO TO 10
      WRITE(IODBUG,7002) N
 7002 FORMAT(/2X,'CONTENTS OF PO ARRAY.    NUMBER OF VALUES = ',I2)
      WRITE(IODBUG,7004) (PO(I),I=1,N)
 7004 FORMAT(' ',10(1X,F10.3))
C
C-----------------------------------------------------------------------
C  SELECT REQUIRED VARIABLES FROM FFG PARAMETER RECORD
C
   10 CALL RPPREC(IDFFG,ITYFFG,IPTR,MFFG,FFG,NFILL,IPTRNX,ISTAT)
      IF(IBUG.GT.0) WRITE(IODBUG,7010) IDFFG,ITYFFG,IPTR,ISTAT
 7010 FORMAT(5X,'FFGID=',2A4,2X,'ITYFFG=',A4,2X,'IPTR=',I5,2X,'ISTAT=',
     . I2)
      IF(IBUG.NE.0) THEN
        NUMFFG = MFFG
        WRITE(IODBUG,7030) MFFG,NUMFFG,IPTR
        WRITE(IODBUG,7035) (FFG(I),I=1,MFFG)
      ENDIF
      IF(ISTAT.EQ.0) GO TO 100
      CALL PSTRDC(ISTAT,ITYFFG,IDFFG,IPTR,MFFG,NFILL)
      IFATAL = 1
      GO TO 8000

  100 ISLOC = FFG(14)
      IRLOC = 42 + IDUR*8
      NCORR = FFG(IRLOC+4)
      IF(ISLOC.GT.0) NCOSN = FFG(ISLOC+4)
C-----------------------------------------------------------------------
C  SET STARTING LOCATION FOR CURVES AND STORE LSTCMPDY IN FFG ARRAY
      IFF = 18
      LSTCMPDY = (LDACPD-1)*24+LHRCPD
      FFG(17)  = LSTCMPDY+0.01
C-----------------------------------------------------------------------
C  CHECK TO SEE IF SNOW MODEL IS AVAILABLE
      AESC  = 0.
      TWE   = 0.
      TMPRS = -999.0
      IF(NOSNOW.EQ.1.OR.NOPSN.EQ.0) GO TO 130
CC      IF(NOPSN.EQ.19) GO TO 120
C.......................................................................
C  FOR SNOW MODEL, GET INITIAL INFO FOR COMPUTING TEMPERATUE LAPSE RATE

  120 IDTA  = PS(14)
      LPM   = PS(25)
      LTAPM = PS(27)
      LAEC  = PS(30)
      IF(LTAPM.EQ.0) GO TO 130
      TAELEV = PS(LTAPM)
      ELEV   = PS(LPM+1)
      EDIFF  = (TAELEV-ELEV)*0.01
      TALMAX = PS(LTAPM+1)
      TALMIN = PS(LTAPM+2)
      DIFF   = TALMAX-TALMIN
      FLOCAL = LOCAL
C-----------------------------------------------------------------------
C  GET THE RAINFALL-RUNOFF MODEL PARAMETER/CARRYOVER DATA
C-----------------------------------------------------------------------
  130 NOPRR = PO(21)
      DO 132 I=1,6
      IF(IRROP(I).EQ.NOPRR) GO TO 134
  132 CONTINUE
  134 LOPRR = I
      GOTO(200,140,200,150,160,170), LOPRR
C.......................................................................
C  API-CONT MODEL (NO. 24)
  140 CALL RCON32(PR,CR,AIADJ,IVOPT,IFRZE,LWE,LSC,IPRINT,ITWE,
     +            ITSC,LTA,ITTA,TAVG)
      PE = 0.
CC      AESC=0.
CC      WE=0.
      TMPRR = 0.
      STRA = 0.0
      GOTO 200
C.......................................................................
C  API-CIN MODEL (NO. 33)
  150 CALL RCIN32(CR,FI,STRA,STRO)
      IF(IBUG.GT.0) WRITE(IODBUG,7040) FI,STRA,STRO
 7040 FORMAT(9X,'FI=',F5.2,5X,'STRA=',F5.2,5X,'STRO=',F5.2,/)
      GOTO 200
C.......................................................................
C  API-SLC MODEL (NO. 34)
C 160 CALL RSLC32(CR,FI,STRA,STRO)
C     GOTO 200
C.......................................................................
C  API-HAR MODEL (NO. 35)
  160 CALL RHAR32(PR,CR,FI,STRA,STRO,RFCTR)
      IF(IBUG.GT.0) WRITE(IODBUG,7060) FI,STRA,STRO,RFCTR
 7060 FORMAT(9X,'FI=',F5.2,5X,'STRA=',F5.2,5X,'STRO=',F5.2,
     +       5X,'RFCTR=',F6.3,/)
      GOTO 200
C.......................................................................
C  API-HFD MODEL (NO. 43)
  170 CALL RHFD32(PR,CR,FI,STRA,STRO,RFCTR,NREL)
      IF(IBUG.GT.0) WRITE(IODBUG,7070) FI,STRA,STRO,RFCTR,NREL
 7070 FORMAT(9X,'FI=',F5.2,5X,'STRA=',F5.2,5X,'STRO=',F5.2,
     +       5X,'RFCTR=',F6.3,5X,'NREL=',I2,/)
C-----------------------------------------------------------------------
C
C  FIND THE NO. OF HOURS IN THE LAST COMPUTED DAY & THE NO. OF
C  TIME PERIODS
  200 IF(NOPSN.NE.0.AND.NOSNOW.EQ.0) NDHR = (LDACPD-IDADAT)*24/IDTA
      NDUR = 3 + IDUR
      NTIM = 2
C_______________________________________________________________________
C  LOOP 1400 ITERATES THRU EACH DURATION IN ARRAY
C-----------------------------------------------------------------------
  205 DO 1400 M=1,NTIM
C
      IF(MTHRSH.EQ.1) IDT=1.
      IF(MTHRSH.EQ.2) IDT = ITFFG(NDUR)
      IF(MTHRSH.GT.2) IDT = ITFFG(M)

      FIDT = IDT
      NXHR = LHRCPD + IDT
      NXDA = LDACPD
      IF(NXHR.GT.24) NXDA = NXDA + 1
      IF(NXHR.GT.24) NXHR = NXHR - 24
      CALL MDYH1(NXDA,NXHR,KMO,KDA,KYR,KHR,NOUTZ,NOUTDS,TZCODE)
C.......................................................................
C  GET AIR TEMPERATURE INFORMATION AND LAPSE RATES FOR SNOW MODEL

      IF(NOSNOW.EQ.1.OR.NOPSN.EQ.0) GO TO 300
      TALR = -999.0
      IF(LTAPM.EQ.0) GOTO 230
      TI = LHRCPD + 0.5*IDT
      TL = TI + FLOCAL
      IF(TL.GT.24.0) TL = TL - 24.0
      IF(TL.LT.15.0) GO TO 220
      TALR = TALMAX - ((TL-15.0)/15.0)*DIFF
      GO TO 230
  220 IF(TL.GT.6.0) GO TO 225
      TALR = TALMIN + ((6.0-TL)/15.0)*DIFF
      GO TO 230
  225 TALR = TALMIN + ((TL-6.0)/9.0)*DIFF
C.......................................................................
C  FIND THE AVERAGE AIR TEMPERATURE FOR SNOW MODEL DURING THIS PERIOD

  230 IHR1 = (LHRCPD/IDTA)*IDTA+IDTA
      IHR2 = ((LHRCPD+IDT)/IDTA)*IDTA+IDTA
      IS   = LHRCPD
      I2   = LHRCPD+IDT
      TMPSN = 0.
      NOTMP = 0
      IF(LAEC.GT.0) TMPRS=0.
      DO 240 I=IHR1,IHR2,IDTA
        LLTA = NDHR + I/IDTA
        IEND = I
        IF(IEND.GT.I2) IEND = I2
        TMPSN = TMPSN + ((IEND-IS)/FIDT)*TASN(LLTA)
        IF(LAEC.EQ.0.OR.NOTMP.NE.0) GOTO 235
        TMPRS = TMPRS + ((IEND-IS)/FIDT)*RSTS(LLTA)
        NOTMP = IFMSNG(RSTS(LLTA))
        IF(NOTMP.EQ.0) GO TO 235
        TMPRS = -999.
CC        GO TO 242
  235   IS = I
  240 CONTINUE
  242 IF(LTAPM.NE.0) TMPSN = TMPSN + TALR*EDIFF

      CALL MDYH1(LDACPD,LHRCPD,LSMO,LSDA,LSYR,LSHR,100,0,TZ)
      IDN = IDANG(LSMO) + LSDA
      IMN = LSMO
CEA SET VALUE OF IYR SO THAT ALL PACK19 ARGUMENTS WILL BE DEFINED.
CEA  NOT ACTUALLY USED IN PACK19 COMPUTATIONS
      IYR = LSYR
C-----------------------------------------------------------------------
C  CHECK FOR DEBUG OUTPUT
      IF(IBUG.NE.0) WRITE(IODBUG,7120) TMPSN,TALR,TMPRS
 7120 FORMAT(//10X,'AIR TEMPERATURE=',F10.3,3X,' LAPSE RATE=',F10.3,
     * 3X,'RAIN-SNOW ELEVATION=',F10.3)
C-----------------------------------------------------------------------
C  GET CARRYOVER AND PARAMETERS FOR SNOW MODEL
CEA PS,CS, AND IDT ARE INPUT VARIABLES FOR RSNO32 - OTHERS ARE OUTPUT
CEA  SNOW-17 VERSION NUMBER ADDED TO ARGUMENT LIST - VARIABLE 'IVS17'
CEA      CALL RSNO32(PS,CS,PGM,IDT,TWE,AESC)
      CALL RSNO32(PS,CS,PGM,IDT,TWE,AESC,IVS17)
      IFUT = 1
      NDT  = 1
C-----------------------------------------------------------------------
C  GET CARRYOVER AND PARAMETERS FOR RAINFALL/RUNOFF MODEL
C
  300 GOTO(310,320,330,400,400,400), LOPRR
C-----------------------------------------------------------------------
C  SAC-SMA MODEL (NO. 1)
  310 CALL RSAC32(PR,CR,EPDIST,IFRZE,FGCO1,IDT,IPRINT)
      KINT=NXHR/IDT
      DT=FIDT/24.
      STRO=0.
      ETD=0.
C.......................................................................
C============= GET AVERAGE TEMPERATURE IN CASE GROUND IS FROZEN ========
C.......................................................................
      IF(IFRZE.EQ.0) GO TO 400
      GO TO 400
C-----------------------------------------------------------------------
C  API-CONT MODEL (NO. 24)
C.......................................................................
C  FIND AVERAGE AIR TEMPERATURE FOR API-CONT MODEL DURING THIS PERIOD
  320 IF(LTA.EQ.0) GO TO 325
      IHR1=(LHRCPD/ITTA)*ITTA+ITTA
      IHR2=((LHRCPD+IDT)/ITTA)*ITTA+ITTA
      IS=LHRCPD
      I2=LHRCPD+IDT
      TMPRR=0.
      DO 322 I=IHR1,IHR2,ITTA
        LLTA=NDHR+I/ITTA
        IEND=I
        IF(IEND.GT.I2) IEND=I2
        TMPRR=TMPRR+((IEND-IS)/FIDT)*(TARR(LLTA)*1.8+32.0)
        IS=I
  322 CONTINUE
C.......................................................................
C  COMPUTE PED AND Y FOR THIS PERIOD
  325 CALL PEDY32(KMO,KDA,IVOPT,PE,TAVG)
      GO TO 400
C-----------------------------------------------------------------------
C  API-MKC MODEL (NO. 29)
  330 CALL RMKC32(PR,CR,IDT,FI,STRA,STRO)
      IF(IBUG.GT.0) WRITE(IODBUG,7150) IDT,FI,STRA,STRO
 7150 FORMAT(9X,'IDT=',I2,5X,'FI=',F5.2,5X,'STRA=',F5.2,5X,
     +          'STRO=',F5.2,/)
      GO TO 400
C-----------------------------------------------------------------------
  400 GO TO(410,420,440), MTHRSH
C.......................................................................
C  FIND THE RAINFALL CORRESPONDING TO THE MINIMUM RUNOFF
  410 RMIN=0.
      RMAX=0.
      RINC=1.
      RAINE=1.
      RO=ROMIN
      NRFV=1
      GO TO 450
C.......................................................................
C  FIND THE RAINFALL CORRESPONDING TO THE MAXIMUM RUNOFF
  420 RMIN=RAINE
      RMAX=0.
      RINC=2.
      IRAIN=RAINE+3
      RAINE=IRAIN
      RO=ROMAX
      NRFV=1
      GO TO 450
C-----------------------------------------------------------------------
C  SET THE MINIMUM AND MAXIMUM RAINFALL AMOUNT AND ITS INCREMENTAL VALUE
  440 RFINCR = RFMAX - RFMIN
      NRFV=4
C_______________________________________________________________________
C  LOOP 1300 ITERATES PRECIP UNTIL CRITERIAL RUNOFF COMPUTED.
C-----------------------------------------------------------------------
  450 DO 1300 J=1,NRFV
C.......................................................................
C  COMPUTE THE RAINFALL STARTING WITH THE CARRYOVER VALUE
        IF(IBUG.GT.0) WRITE(IODBUG,7190) MTHRSH
 7190   FORMAT(9X,'MTHRSH=',I2)
        ITER = 0
        IF(MTHRSH.GT.2) GO TO 460
  455   RAINM=RAINE*CONV
        RAIMLT=RAINM
        GO TO 470

  460   RAINE=RFMIN+RFINCR*PCIN(J)
        RAINM=RAINE*CONV
        RAIMLT=RAINM

  470   IF(NOSNOW.EQ.1) GO TO 500
C-----------------------------------------------------------------------
C  COMPUTE THE SNOW-MELT USING THE SNOW-17 MODEL
        IF(NOPSN.NE.19) GO TO 500
CEA IYR AND IVS17 ADDED TO ARGUMENT LIST FOR SNO32
CEA        CALL SNO32(IDT,RAINM,TMPSN,TMPRS,TWE,AESC,IFUT,NDT,PGM,IMN,IDN,
CEA     .             RMELT,LDACPD,LHRCPD)
        CALL SNO32(IDT,RAINM,TMPSN,TMPRS,TWE,AESC,IFUT,NDT,PGM,IMN,IDN,
     .         IYR,RMELT,LDACPD,LHRCPD,IVS17)
CEA NOTE: NORMALLY THE KDA/KHR VALUES PASSED TO PACK19 REPRESENT THE END
CEA  OF THE CURRENT COMPUTATIONAL PERIOD.  IN THIS CASE LDACPD/LHRCPD
CEA  (THE BEGINNING OF THE COMPUTATIONAL INTERVAL) IS USED SINCE FOR THE
CEA  FFG OPERATION THE HOUR AT THE END OF THE INTERVAL MAY NOT BE A
CEA  MULTIPLE OF THE SNOW-17 COMPUTATIONAL INTERVAL.  THIS HAS NEGLIGIBLE
CEA  EFFECTS ON THE RESULTS.
        RAIMLT=RMELT
C-----------------------------------------------------------------------
C  CONVERT RAIN/MELT (MM) TO TOTAL RAIN/MELT (IN) FOR API- MODELS
C 500 IF(LOPRR.NE.1) RAIMLT=RFMIN+RAIMLT/25.4
  500 IF(LOPRR.NE.1) RAIMLT = STRA + RAIMLT/CONV
C-----------------------------------------------------------------------
C  COMPUTE THE RUNOFF FROM THE RAIN-MELT USING THE APPROPIATE MODEL
      GOTO(510,520,530,540,550,560), LOPRR
C.......................................................................
C  SAC-SMA MODEL (NO. 1)
C   Save value of ioutyp for later and set it to 0 while doing ffg sac
  510 SAVOUT=IOUTYP
      IOUTYP=0
      CALL SAC32(PR,EPDIST,IFRZE,KINT,DT,ETD,RAIMLT,ROFF,TA,KDA,KHR,
     . KMO,KYR,TZCODE,TWE,AESC)
      ROFF=ROFF/CONV
      IOUTYP=SAVOUT
      GO TO 600
C.......................................................................
C  API-CONT MODEL (NO. 24)
  520 WE = TWE/CONV
      CALL CAPI32(RAIMLT,RS,PFRS,AI,RG,ROFF,PE,AESC,TMPRR,TAVG,WE,
     +            AIADJ,NXHR,KMO,KDA,KHR,IVOPT,IFRZE,LWE,LSC,IDT)
      GO TO 600
C.......................................................................
C  API-MKC MODEL (NO. 29)
  530 CALL RO29(FI,RAIMLT,ROFF)
      GO TO 580
C.......................................................................
C  API-CIN MODEL (NO. 33)
  540 CALL RO33(FI,RAIMLT,ROFF)
      GO TO 580
C.......................................................................
C  API-SLC MODEL (NO. 34)
C 550 CALL RO34(FI,RAIMLT,ROFF)
C     GO TO 580
C.......................................................................
C  API-HAR MODEL (NO. 35)
  550 CALL RO35(FI,RAIMLT,ROFF)
      ROFF=ROFF-STRO
      RAIMLT = RAIMLT - STRA
      IF(RFCTR.GT.0.) ROFF=ROFF*RFCTR
      GO TO 600
C.......................................................................
C  API-HFD MODEL (NO. 43)
  560 CALL RO43(NREL,FI,RAIMLT,ROFF)
      ROFF = ROFF - STRO
      RAIMLT = RAIMLT - STRA
      IF(RFCTR.GT.0) ROFF = ROFF*RFCTR
      GOTO 600
C-----------------------------------------------------------------------
C  COMPUTE THE INCREMENTAL RUNOFF AND INCREMENTAL RAINFALL
  580 ROFF=ROFF-STRO
      RAIMLT = RAIMLT - STRA
  600 IF(IBUG.GT.0) THEN
        IF(MTHRSH.NE.3) THEN
           WRITE(IODBUG,7210) J,RAIMLT,RAINE,ROFF,RO
 7210      FORMAT(12X,'J=',I6,5X,'RAIMLT=',F6.2,5X,'RAINE=',F5.2,
     +            5X,'ROFF=',F5.2,5X,'TARG RO=',F5.2)
        ELSE
           WRITE(IODBUG,7220) J,RAIMLT,RAINE,ROFF
 7220      FORMAT(12X,'J=',I6,5X,'RAIMLT=',F6.2,5X,'RAINE=',F5.2,
     +            5X,'ROFF=',F5.2)
        ENDIF
      ENDIF

      GO TO (700,700,900), MTHRSH
C-----------------------------------------------------------------------
C  CHECK FOR MINIMUM THRESHOLD RUNOFF & ADJUST RAINE USING BISECTION
C  METHOD
  700 RDIF = ROFF - RO
C
      IF(ABS(RDIF).LT.0.01) THEN
C  SAVE MINIMUM OR MAXIMUM THRESHOLD RUNOFF
        GOTO 800
C
      ELSE IF(RDIF.GT.0.) THEN
C
C  RUNOFF GREATER THAN MAXIMUM THRESHOLD RUNOFF...SET RMAX
C  AND USE BISECTION
        RMAX = RAINE
        RAINE = (RMIN + RMAX)/2.
        IF(IBUG.GT.0) WRITE(IODBUG,7260) RDIF,RMAX,RAINE
 7260   FORMAT(9X,'A: RDIF=',F6.2,2X,'RMAX=',F5.2,
     +         3X,'NEXT RAINE=',F5.2,/)
        IF(ABS(RMIN-RMAX).LT.0.00001) GOTO 800
C
      ELSE IF(ABS(RDIF).LE.RINC) THEN
C
C  RUNOFF LESS THAN MINIMUM THRESHOLD RUNOFF...SET RMIN AND
C  USE BISECTION
        RMIN = RAINE
        IF(RMAX.LE.0.0001) RAINE = RAINE + RINC/2.
        IF(RMAX.GT.0.)     RAINE = (RMIN + RMAX)/2.
        IF(IBUG.GT.0) WRITE(IODBUG,7270) RDIF,RMIN,RAINE
 7270   FORMAT(9X,'B: RDIF=',F6.2,2X,'RMIN=',F5.2,
     +         3X,'NEXT RAINE=',F5.2,/)
        IF(ABS(RMIN-RMAX).LT.0.00001) GOTO 800
C
      ELSE
C
C  RUNOFF LESS THAN MINIMUM THRESHOLD RUNOFF ... INCREMENT BY RINC INCH
        RMIN = RAINE
        RAINE = RAINE + RINC
        IF(IBUG.GT.0) WRITE(IODBUG,7280) RDIF,RMIN,RAINE
 7280   FORMAT(9X,'C: RDIF=',F6.2,2X,'RMIN=',F5.2,
     +         3X,'NEXT RAINE=',F5.2,/)
      ENDIF
C  LIMIT NUMBER OF ITERATIONS TO FIND RUNOFF
      ITER = ITER + 1
      IF(ITER.GT.MXITER) THEN
        RFMAX = MAX(RAINE,RMAX)
        CALL WARN
        WRITE(IPR,7290) MXITER,IDFFG
 7290   FORMAT(10X,'**WARNING**  EXCEEDED MAX NUMBER OF ',
     +         'ITERATIONS:',I4,', FOR ',2A4)
        GOTO 800
      ENDIF
      GO TO 455
C.......................................................................
C  SAVE MINIMUM OR MAXIMUM THRESHOLD RUNOFF
  800 IF(MTHRSH.EQ.1) RFMIN = RAINE
      IF(MTHRSH.EQ.2) RFMAX = RAINE + 0.1
      MTHRSH = MTHRSH + 1
      GO TO 1300

C.......................................................................
C  STORE THE RAINFALL/RUNOFF VALUES IN THE FFG ARRAY
  900 FFG(IFF)=RAINE
      FFG(IFF+1)=ROFF
      IF(IBUG.GT.0) WRITE(IODBUG,7300) IFF,RAINE,ROFF
 7300 FORMAT(12X,'IFF=',I4,5X,'RAINE=',F7.2,5X,'ROFF=',F5.2,/)
      IFF=IFF+2
C.......................................................................
 1300 CONTINUE
 1400 CONTINUE
      IF(NTIM.EQ.NDUR) GO TO 1500
      NTIM=NDUR
      IF(MTHRSH.EQ.3) GO TO 205
C-----------------------------------------------------------------------
C  FILL THE FFG ARRAY WITH RAINFALL/RUNOFF PARAMETERS FOR CO
C  (5 VALUES: MODEL TYPE (2), MODEL NAME (2), & NO. OF CO VALUES
 1500 DO 1510 I=1,4
      I1=I-1
      FFG(IRLOC+I1)=PO(09+I1)
 1510 CONTINUE
      FFG(IRLOC+4)=NCORR+0.01
      IRLC=IRLOC+4
      GO TO (1520,1530,1540,1550,1560,1570), LOPRR
C.......................................................................
C  FILL THE FFG ARRAY WITH RAINFALL/RUNOFF CARRYOVER VALUES
C  SAC-SMA MODEL (UZTWC,UZFWC,LZTWC,LZFSC,LZFPC,ADIMC,FGCO1 )
 1520 N = NCORR - 1
      DO 1525 I=1,N
      FFG(IRLC+I)=CR(I)
 1525 CONTINUE
      FFG(IRLC+7) = FGCO1
      GO TO 1600
C.......................................................................
C  API-CONT MODEL (API,SMI,BFSC,BFI,AEI OR ATI,FI,FEI,IVOPT)
 1530 N = NCORR - 1
      DO 1535 I=1,N
      FFG(IRLC+I) = CR(I)
 1535 CONTINUE
      FFG(IRLC+8) = IVOPT
      IF(IFRZE.GT.0) GO TO 1600
      FFG(IRLC+6) = 32.
      FFG(IRLC+7) = 0.
      GO TO 1600
C.......................................................................
C  API-MKC (API,RANCO,ROCO,AICO,AI,NEWSTM)
C  NOTE:  IN THE API_MKC OPERATION THE CARRYOVERS ARE ALL INTEGER AS
C  FOLLOWS:  IAPI,IRANCO,IROCO,IAICO,IAI,NEWSTM BUT FOR STANDARDIZATION
C  WITH THE OTHER API MODELS THE VALUES ARE CHANGED TO REAL EXCEPT
C  NEWSTM FOR THIS CARRYOVER TRANSFER.
 1540 FFG(IRLC+1) = CR(5)*0.01
      FFG(IRLC+2) = CR(2)*0.01
      FFG(IRLC+3) = CR(4)*0.01
      FFG(IRLC+4) = CR(3)*0.1
      FFG(IRLC+5) = CR(6)*0.1
      FFG(IRLC+6) = CR(1)
      GO TO 1600
C.......................................................................
C  API-CIN (API,RANCO,ROCO,AICO,AI,NEWSTM,AVGT,TC)
 1550 FFG(IRLC+1) = CR(5)
      FFG(IRLC+2) = CR(2)
      FFG(IRLC+3) = CR(4)
      FFG(IRLC+4) = CR(3)
      FFG(IRLC+5) = CR(6)
      FFG(IRLC+6) = CR(1)
      FFG(IRLC+7) = CR(9)
      FFG(IRLC+8) = CR(11)
      GOTO 1600
C.......................................................................
C  API-SLC MODEL
C1550 GO TO 1600
C.......................................................................
C  API-HAR MODEL (API,SRAIM,SRO,SAI,YAI,SAEI,YAEI,YAPI)
 1560 FFG(IRLC+1) = CR(4)
      FFG(IRLC+2) = CR(7)
      FFG(IRLC+3) = CR(8)
      FFG(IRLC+4) = CR(6)
      FFG(IRLC+5) = CR(3)
      FFG(IRLC+6) = CR(5)
      FFG(IRLC+7) = CR(2)
      FFG(IRLC+8) = CR(1)
      GOTO 1600
C.......................................................................
C  API-HFD MODEL (API,SRAIM,SRO,SAI,CAI,SAEI,CAEI,CAPI)
 1570 FFG(IRLC+1) = CR(4)
      FFG(IRLC+2) = CR(7)
      FFG(IRLC+3) = CR(8)
      FFG(IRLC+4) = CR(6)
      FFG(IRLC+5) = CR(3)
      FFG(IRLC+6) = CR(5)
      FFG(IRLC+7) = CR(2)
      FFG(IRLC+8) = CR(1)
C-----------------------------------------------------------------------
C  FILL FFG ARRAY WITH SNOW PARAMETERS
C  (5 VALUES: MODEL TYPE (2), MODEL NAME (2), & NO. OF CO VALUES
 1600 IF(ISLOC.EQ.0) GO TO 1700
      DO 1610 I=1,4
      I1=I-1
      FFG(ISLOC+I1)=PO(13+I1)
 1610 CONTINUE
      FFG(ISLOC+4)=NCOSN+0.01
      ISLC=ISLOC+4
C.......................................................................
C  FILL THE FFG ARRAY WITH SNOW CARRYOVER VALUES
C  (WE,NEGHS,LIQW,TINDEX,ACCMAX,SB,SBAESC,SBWS,STORGE,AEADJ,EXLAG(N)
C  WHERE N IS 2 + (5/TIME INTERVAL PRECIP DATA)  OR  N = NCOSN - 10 )
CEA NOTE: FFG PARAMETER ARRAY (PPPDB) IS ONLY SET UP TO HOLD THE
CEA  ORIGINAL SNOW-17 CARRYOVER.  DEPENDING ON THE SNOW-17 VERSION
CEA  NUMBER THE SNOW MODEL CAN HAVE ADDITIONAL CARRYOVER FOR DEPTH,
CEA  SNOWPACK TEMPERATURE, AND THE PREVIOUS PERIOD AIR TEMPERATURE.
CEA  IF THESE VALUES ARE NEEDED BY PROGRAMS THAT USE THE PPPDB FFG
CEA  PARAMETER ARRAY CHANGES WILL HAVE TO BE MADE TO THE FFG PARAMETER
CEA  INPUT ROUTINE (PIN32) SUCH THAT THE FFG PARAMETER ARRAY CAN HOLD
CEA  ALL THE SNOW-17 CARRYOVER VALUES.
      DO 1620 I=1,NCOSN
      FFG(ISLC+I)=CS(I)
 1620 CONTINUE

C-----------------------------------------------------------------------
C  PRINT THE CONTENTS OF THE FFG ARRAY
 1700 IF(IBUG.EQ.0) GO TO 1800
      NUMFFG=IRLC+NCORR
      IF(ISLOC.GT.0) NUMFFG=ISLC+NCOSN
      WRITE(IODBUG,7030) MFFG,NUMFFG,IPTR
 7030 FORMAT(//2X,'CONTENTS OF THE FFG ARRAY:  MFFG=',I5,'  NUMFFG=',I5,
     * '  IPTR=',I5)
      WRITE(IODBUG,7035) (FFG(I),I=1,NUMFFG)
 7035 FORMAT(2X,10F10.3)
C-----------------------------------------------------------------------
C  STORE THE FFG ARRAY INTO THE PPPDB
 1800 ISTAT=0
      CALL WPPREC(IDFFG,ITYFFG,MFFG,FFG,IPTR,ISTAT)
      IF(ISTAT.NE.0) CALL WRST32(ISTAT,ITYFFG,IDFFG)
      IF(ISTAT.EQ.0) IWRPPP=1
      CALL RPPREC(IDFFG,ITYFFG,IPTR,MFFG,FFG,NFILL,IPTRNX,ISTAT)
      IF(ISTAT.EQ.0) WRITE(IPR,50) IDFFG
   50 FORMAT(/,6X,'COMPLETED FFG FOR ',2A4)
      IF(IBUG.EQ.0) GO TO 8000
      WRITE(IODBUG,7030) MFFG,NUMFFG,IPTR
      WRITE(IODBUG,7035) (FFG(I),I=1,NUMFFG)
C.......................................................................
 8000 RETURN
      END
