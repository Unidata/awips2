C MODULE EX22
C
C***********************************************************************
C          HYDROLOGIC FORECAST SYSTEM - HFS
C
C     CONTROL PROGRAM FOR REAL-TIME STOCHASTIC-DYNAMIC FLOOD PREDICTION
C      1987 VANCOUVER,CANADA, WMO INTERCOMPARISON PROJECT
C
C     PRELIMINARY VERSION TO BE USED FOR INSTALATION AT THE UBC COMPUTER
C
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, UI, NOVEMBER-1986
C
C  UNDER NOAA COOPERATIVE AGREEMENT NA86AA-H-HY126, 1986-1987
C
C  REVISION HISTORY FOR THIS MODULE:
C
C  HFS VERSION 1.0 - original stand-alone main program module HFS_CO.FOR
C
C  HFS VERSION 1.1 - modified to be the execution routine for NWS FC
C
C  HFS VERSION 1.2 - added efficiency changes
C
C  SS-SAC VERSION 1.3:
C 7-24-89  4:42:33 pm, J.C..:
C    changed code and syntax for IBM OS FORTRAN IV (H-EXTENDED)
C
C  SS-SAC VERSION 1.4:   04-26-2000, KPG
C    modified to write variance data to new output time series
C
C  SS-SAC VERSION 1.5:   10-24-2000, KPG
C    corrections to provide the anticipated 13 lines of forecast output,
C    v.1.4 overlooked these corrections and produced only 12 lines.
C
C  SS-SAC VERSION 1.6:   04-19-2001, KPG
C    changes to allow a time series of factors to be passed to FLWS22
C    to multiply the coefficient of variation of the MAPF for the
C    forecast time steps with the values of either 1 or as changed
C    by real time mods.
C
C  SS-SAC VERSION 1.7:   08-27-2002, JAS
C    changed MAXNF and related variables to 744, allowing for the same
C    max forecasting steps as the NWSRFS limit of 31 days of 24 steps.
C
C***********************************************************************
C
       SUBROUTINE EX22 (P0,C0,DP,DE,DD,DS,DV,DF)
C
C************************* PARAMETER VARIABLES *************************
C
C...parameter input
       REAL P0(1)
C...initial & final carryover
       REAL C0(1)
C...time series 1, input, precip time series data
       REAL DP(1)
C...time series 2, input, evapotranspiration data
       REAL DE(1)
C...time series 3, input, observed discharge data
       REAL DD(1)
C...time series 4, output, to return estimated discharge for time
C... steps during computational period and predicted discharge for
C... time steps during forecast period, also for stdev of sim discharge
       REAL DS(1)
       REAL DV(1)
C
       REAL DF(1)
C...time series of factors for MAPF coef of variation changes in FLWS22
C
C
C*************** VARIABLES STORED IN PARAMETER & CARRYOVER ARRAYS ******
C...parameters and pointers to their storage location in P0 & C0 arrays
C
C THIS SCHEME
C    DECLARES A POINTER VARIABLE FOR EVERY INPUT PARAMETER AND CARRYOVER
C     VARIABLE FOR THE SS-SAC OPERATION.  THE CONTENTS OF EACH POINTER
C     VARIABLE INDICATES WHERE ITS CORRESPONDING PARAMETER/
C     CARRYOVER VARIABLE'S STORAGE BEGINS IN THE FC P0 OR C0 ARRAY.
C
C-----------------------------------------------------------------------
C
C A. VARIABLES STORED IN P0 ARRAY, BUT NOT IN NON-FC COMMON, & THEIR POI
C
C*** parameter variables and pointers to their locations in P0 array.
C   The parameter
C     variables are declared with a certain type, dimension, and name.
C     Each variable has a pointer associated with it.  The name of the
C     pointer is the same as the variable name execpt that a P is added
C     to the start of the name.  This pointer contains the index of the
C     array where the start of storage for the pointers corresponding
C     parameter variable begins.
C   Variables which indicate the dimension size of an array have the
C     same name as the array but with 'S' for the 1st character.
C
C...version (revision) number of the SS-SAC operation and its parameter
C... pointer assignments
       REAL OPVER
       INTEGER POPVER
C
C...run label
       REAL ILBL(15)
       INTEGER PILBL
C
C...model reference number - FC Operation Number
       INTEGER MRNO, PMRNO
C
C...index of last element of SS-SAC P array used for model param storage
       INTEGER NCC1, PNCC1
C
C...pointer to where number of soil parameters used is stored in P
       INTEGER PNPA
C
C...pointer to where number of Q parameters used is stored in P
       INTEGER PNQ
C
C...number of router parameters used
       INTEGER NR, PNR
C
C...coefficient of generic non-linear routing reservoir
       INTEGER PALPD
       REAL ALPD
C
C...constant s.d. & coefficient of variation of DISCHARGE
       INTEGER PR1
       REAL R1
       INTEGER PR2
       REAL R2
C
C...SACRAMENTO MODEL INITIAL CONDITIONS
       INTEGER PSCM, SSCM
       REAL SCM(6)
C
C...square root of diag elements of SACRAMENTO init state covar matrix
       INTEGER PSCMCV, SSCMCV
       REAL SCMCV(12)
C
C...number of soil states of Sacremento model
       INTEGER PNORG
       INTEGER NORG
C
C...precipitation time-series internal identifier name
       REAL TSP(2)
       INTEGER PTSP
C
C...time series of factors for MAPF coef of variation
	 REAL TSFA(2)
	 INTEGER PTSFA
C
C...discharge time-series internal identifier name
       REAL TSD(2)
       INTEGER PTSD
C
C...evapotranspiration time-series internal identifier name
       REAL TSE(2)
       INTEGER PTSE
C
C...simulated discharge time-series internal identifier name
       REAL TSS(2)
       INTEGER PTSS
C
C...sim stdev discharge time-series internal identifier name
       REAL TSSD(2)
       INTEGER PTSSD
C
C...precipitation time-series data type code
       REAL DTCP
       INTEGER PDTCP
C
C...MAPF CV factor time series data type code
	 REAL DFAC
	 INTEGER PDFAC 
C
C...discharge time-series data type code
       REAL DTCD
       INTEGER PDTCD
C
C...evapotranspiration time-series data type code
       REAL DTCE
       INTEGER PDTCE
C
C...simulated discharge time-series data type  code
       REAL DTCS
       INTEGER PDTCS
C
C...sim stdev discharge time-series data type  code
       REAL DTCSD
       INTEGER PDTCSD
C
C...interval between ET time-series data values
       INTEGER PNHRSE
C
C...interval between observed or simulated Discharge time-series data
       INTEGER PNHRSD
C
C...pointer to precipitation constant standard deviation value in P
       INTEGER PPCSD
C
C...pointer to precipitation coefficient of variation in P
       INTEGER PPCV
C
C...pointer to evaporation constant deviation value in P
       INTEGER PECSD
C
C...pointer to evaporation coefficient of variation value in P
       INTEGER PECV
C
C...pointer to location of first diag term of model error SD in P
       INTEGER PDTSD
C
C...pointer to number of diag terms of model error SD
       INTEGER PNDTSD
C
C...pointer to location of 1st soil parameter in P
       INTEGER PSP
C
C...pointer to location of 1st router parameter in P
       INTEGER PRP
C
C...switch to select:
C...  if ETSW=1 => ET potential time series & 12 monthly correction fact
C...  if     =0 => 12 long-term mean, actual ET demand values
       INTEGER PETSW
C
C...pointer to where 12 ET numbers are stored in P0 parameter array
C... these numbers are:  -- 12 monthly correction factors if ETSW =1
C...                     -- 12 monthly demand means if ETSW = 0
       INTEGER PETDAT
C
C...pointer to initial conditions discharge value
       INTEGER PXDIS1
C
C...total amount of space in P0 array required for parameters
C... = PP + P0(PNCC1) - 1 elements
       INTEGER TOTALP
C
C-----------------------------------------------------------------------
C
C B. POINTERS FOR VARIABLES STORED IN BOTH P0 AND IN NON-FC COMMON
C
C...time interval, in hours, between observed data or forecast steps
C... and should be equal to the interval between precip & discharge data
       INTEGER PNHST
C
C...location of SS-SAC parameter array (for exec subroutine) within P0
       INTEGER PP
C
C...catchment area in sqkm
       INTEGER PAREA
C
C... exponent parameter of generic non-linear routing reservoir
       INTEGER PXM
C
C...coefficient parameter of generic non-linear routing reservoir
       INTEGER PALP
C
C...???
       INTEGER PQ
C
C...max coeff of variation allowed for stat. linearization
C    (if = 0, no stat lin is performed)
       INTEGER PCFVMX
C
C...coeff of input component of model error covariance
       INTEGER PALINP
C...coeff of parameter component of model error covariance
       INTEGER PALPAR
C
C...standard deviation of rainfall-runoff model parameter estimates
       INTEGER PPSTDV
C
C...number of subdivisions
       INTEGER PSUBS
C
C-----------------------------------------------------------------------
C
C C. POINTERS FOR VARIABLES STORED IN BOTH NON-FC COMMON AND IN C0
C
       INTEGER PY
C
       INTEGER PYST
C
C
C...total storage required in C0 for carryover
C... = (NR+NORG) + (NR+NORG)*(NR+NORG-1)/2 + 1 (for initial discharge)
       INTEGER TOTALC
C
C***********************************************************************
C
C...non-FC common blocks
       COMMON/STAT22/ Y(91)
       COMMON/IUPD22/ INDX
       COMMON/STTU22/ YST(91)
       COMMON/SUBD22/ SUBS
       COMMON/STAN22/ NC
       COMMON/SPDN22/Q(12)
C***J.C., 890810
C      COMMON/PMOD22/ P(1000)
       COMMON/PMOD22/ P(70)
       COMMON/ACCU22/ TOL
       COMMON/OBSN22/ R(2)
CKPG 12/18/00 ADDED TIME SERIES OF MAPF CV FACTORS
CJAS 20020827 changed max from 50 to 744 to accommodate 31 x 24 steps
       COMMON/OBSZ22/ ZP(744),ZE(744),ZH,ZF(744)
CKPG 12/2/97 ADDED FORECAST ST DEV ZSTDPD
       COMMON/OBSD22/ ZPD,ZED,ZHD,ZHPRED,ZSTDPD,ZHEST,KDA,
     +    KDAP,KHR,KHRP,
     +    NDM, ZDD
C***J.C.,890810
       COMMON/PFLAGS/ IFIRST, NCTIM
       COMMON/SQKM22/ AREA
       COMMON/TIMC22/ NHST
       COMMON/DATS22/ NYRMO,NDAHR
       COMMON/VARI22/ VARCHN
       COMMON/VARA22/ CFVMX
       COMMON/PARM22/XM,ALP(6)
       COMMON/WRWM22/IARCH,NR12,IST,NS,DIVIS
       COMMON/CVPR22/ALINP,ALPAR,PSTDV(20)
C
C******************* OTHER VARIABLES ***********************************
C
       DIMENSION X1(12),CX1(12,12)
       INTEGER*4 SNAME(2)
       INTEGER IMIS
       INTEGER NF1
       INTEGER LF
       INTEGER IRD
       INTEGER ISG, ISG1
       INTEGER IK, IJ
       INTEGER N1, N2
       INTEGER NSTEP
C
C...#of mean+covariance carryover states
       INTEGER NDM
C
C...debugging flags
       INTEGER IDB1, IDB2, IDB
C
C...NS = number of times through update/predict main loop, with NF
C... predictions the last time (NSth) through the loop
       INTEGER NS
C
C...number of observed discharge data points
       INTEGER NOBSV
C
C...number of forecast steps to predict
       INTEGER NFORE
C
       INTEGER*4 ITM1, ITM2, ITM3
C
C...starting yr, mo, day, hr of run/computation period/1st time step
       INTEGER IIYR,IIMO,IIDAY,IIHR
C...starting yr, mo, day, hr of forecast period/1st forcast step
       INTEGER IIYRF, IIMOF, IIDAF, IIHRF
C...yr, mo, day, hr of previous time step
       INTEGER IIYRP, IIMOP, IIDAP, IIHRP
C
C...general purpose yr, mo, day, hr variables for internal conversions
       INTEGER YR, MO, DA, HR
C
C...internal clock day, hour at any given time step
       INTEGER*4 KDA, KHR
C...internal clock day, hour at previous time step
       INTEGER*4 KDAP, KHRP
C
C...index into ICDAY & ICHOUR indicating times for saving carryover
       INTEGER NCTIM
C
C...index in the time-series arrays of the 1st data point to use
       INTEGER IOFSET
C
C...our time zone code (4-char)
       REAL TZC
C
       REAL XMAP, XEVAP, XDISCH, ZQM
       REAL ZHCMS
       REAL XDM10
       REAL ZQM0
       REAL TEND
C
C...maximum allowable precip value (mm)/dt
       REAL PMAX
C
C...if ETSW = 1, holds 12 monthly correction factors
C...        = 0, holds 12 monthly mean daily demand values
       REAL ETDAT(12)
       INTEGER ETSW
C
C...intervals for ET & Discharge time-series data
       INTEGER NHRSE, NHRSD
C
C...dimension capacity of arrays to hold time-series data while forecast
       INTEGER MAXNF
C...dimension of local parameter array
       INTEGER MAXP
C
C...internal operation identification number
       INTEGER NOPER
C...name of operation (for use by IFBUG)
       REAL*4 OPNAM1, OPNAM2
C...current version of execution subroutine
       REAL*4 CURVER
C
C**************** FC COMMON BLOCKS *************************************
C
C...input/output logical unit numbers:
C...  IN  = for card image input
C...  IPR = for printed output
C...  IPU = for punched output
       COMMON /IONUM/IN,IPR,IPU
C
C...for debugging:
       COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C...for timing information:
C RUN - ENTIRE CALCULATION PERIOD (NS + NF)
C
C       IDA    - beginning day of computations
C       IHR    - beginning hour (1st time step) of computations
C       LDA    - last day of computations
C       LHR    - hour at last time step
C       LDACPD - last day of observed data
C       LHRCPD - hour of last observed data value
C       NOW(5) - has current clock time (should not need to be used)
C       LOCAL  - used in timing utilities (should not be needed)
C       NLSTZ  - "                      "
C       NOUTZ  - default output time zone code - useful in printouts
C       NOUTDS - default output daylight
C       IDADAT - day corresponding to 1st data value in TS array,
C                 corresponding to 1st data TS period in that day, used
C                 compute offset into TS series array to start of data
C                 to be used
C
C  notes: NF could be 0 (LDACPD >= LDA)
C         NS could be 0 (LDACPD <= IDA)
C         all clock times internal (Julian dates from 1900)
C
       COMMON /FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
     +  NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,LDA,LHR,IDADAT
C
C...control information for saving carryover:
       COMMON /FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
C
C...needed for execution subroutine that generate printer output other
C... than error messages & debug output
       COMMON /FNOPR/NOPROT
C
C...KPG  character variable for MAP CV type checks
	CHARACTER*6 MAPCVT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/ex22.f,v $
     . $',                                                             '
     .$Id: ex22.f,v 1.5 2002/10/10 16:27:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C CALL MYDH1 (A1,A2,...A9)    where:
C   A1 (input, INTEGER) - julian day
C   A2 (input, INTEGER) - 24 hour
C   A3 (OUTPUT,INTEGER) - month
C   A4 (OUTPUT,INTEGER) - day
C   A5 (OUTPUT,INTEGER) - year
C   A6 (OUTPUT, INTEGER) - hour
C   A7 (INPUT, INTEGER) - time zone code NOUTZ
C   A8 (INPUT, INTEGER) - daylight savings switch NOUTDS
C   A9 (OUTPUT,REAL   ) - 4-char time zone code
C
C***********************************************************************
C
       DATA SNAME/4HEX22,4H    /
       DATA OPNAM1/4HHFS1/
       DATA OPNAM2/4HHFS2/
       DATA NOPER/22/
CJAS...MAXNF changed from 50 to 744 to accommodate 31 days of 24 steps
       DATA MAXNF/744/
       DATA MAXP/70/
C
C
C!!!CHANGE THIS IF PARAMS.INC FILE IS MODIFIED, OR IF THE EX22 CODE
C    IS CHANGED IN A WAY THAT IS DEPENDENT UPON CHANGES TO PIN22
C       DATA CURVER/1.5/
CKPG...CHANGES FOR MAP VARIANCE MODIFIERS, VERSION 1.6
	  DATA CURVER/1.6/
C
C!!!CHANGE THIS IF YOU WANT TO INCREASE THE MAXIMUM ALLOW PRECIP TIME-SE
C   SERIES VALUES
       DATA PMAX/100.0/
C
C***********************************************************************
C
C...should always be the 1st executable statement in FC primary subrouti
C... IBUG will = 1 if debug discharge table is to be printed
       CALL FPRBUG (SNAME,1,NOPER,IBUG)
C      WRITE (IPR,9959) IBUG
C9959  FORMAT(1X,/,'IBUG= ',I2)
       IBUG = 0
C
C-----------------------------------------------------------------------
C
C **LOAD POINTERS TO PARAMETER VARIABLES IN P0
C
       POPVER= 1
       PILBL = POPVER + 1
       PMRNO = PILBL  +15
       PNCC1 = PMRNO  + 1
       PNR   = PNCC1  + 1
       PALPD = PNR    + 1
       PXM   = PALPD  + 1
       PALP  = PXM    + 1
       PSCM  = PALP   + 6
       PSCMCV= PSCM   + 6
       PNORG = PSCMCV +12
       PTSP  = PNORG  + 1
       PTSD  = PTSP   + 2
       PTSE  = PTSD   + 2
       PTSS  = PTSE   + 2
       PDTCP = PTSS   + 2
       PDTCD = PDTCP  + 2
       PDTCE = PDTCD  + 2
       PDTCS = PDTCE  + 2
       PNHST = PDTCS  + 2
       PNHRSE= PNHST  + 1
       PNHRSD= PNHRSE + 1
       PAREA = PNHRSD + 1
       PQ    = PAREA  + 1
       PCFVMX= PQ     +12
       PALINP= PCFVMX + 1
       PALPAR= PALINP + 1
       PPSTDV= PALPAR + 1
       PPCSD = PPSTDV +20
       PPCV  = PPCSD  + 1
       PECSD = PPCV   + 1
       PECV  = PECSD  + 1
       PR1   = PECV   + 1
       PR2   = PR1    + 1
       PDTSD = PR2    + 1
       PNDTSD= PDTSD  + 1
       PNPA  = PNDTSD + 1
       PSP   = PNPA   + 1
       PRP   = PSP    + 1
       PNQ   = PRP    + 1
       PETSW = PNQ    + 1
       PETDAT= PETSW  + 1
       PXDIS1= PETDAT +12
       PSUBS = PXDIS1 + 1
       PTSSD = PSUBS  + 1
       PDTCSD= PTSSD  + 2
CKPG..added pointers for MAPF CV factors
	 PTSFA = PDTCSD + 2
	 PDFAC = PTSFA  + 2
C
       PP    = PDFAC + 1
C
       PY     =   1
C
C-----------------------------------------------------------------------
C
C... returns value of 1 if output is to be printed
       IDB1 = IFBUG(OPNAM1)
C... returns value of 1 if output will be detailed
       IDB2 = IFBUG(OPNAM2)
       IF (IDB1 .EQ. 0) IDB = -1
       IF (IDB1.EQ.1 .AND. IDB2.EQ.0) IDB = 0
       IF (IDB1.EQ.1 .AND. IDB2.EQ.1) IDB = 1
       IBUG = 0
C      IDB  = 0
C
C  ALL INTERNAL COMPUTATIONS ARE DONE IN UNITS:
C         MM  FOR  DEPTH,      DT-HRS  FOR  TIME
C  INPUT DATA SHOULD BE CONVERTED TO THESE UNITS BEFORE PROCESSING
C
C
C**********  RECOVER PARAMETERS NEEDED FOR EXECUTION SUBROUTINE *******
C
       OPVER = P0(POPVER)
       IF (OPVER .EQ. CURVER) GO TO 701
           WRITE (IPR,932) OPVER,CURVER
932        FORMAT(/10X,'**WARNING** POSSIBLE PROBLEMS DUE',
     +      ' TO VERSION CONFLICT!',/,
     +      15X,'PIN22 VERSION ',F4.1,' WAS USED',
     +      ' TO INPUT PARAMETERS, BUT',/,
     +      15X,'EX22  VERSION ',F4.1,' IS IN USE.'/)
           CALL WARN
701    CONTINUE
C
       DO 800 I = 1,15
           ILBL(I) = P0(PILBL+I-1)
800    CONTINUE
C
       NCC1 = INT(P0(PNCC1))
       IF (NCC1 .LE. MAXP) GO TO 702
           WRITE (IPR,9321) OPVER,NCC1,CURVER,MAXP
9321       FORMAT(/10X,'**ERROR** POSSIBLE PROBLEMS DUE',
     +      ' TO VERSION CONFLICT!',/,
     +      15X,'PIN22 VERSION ',F4.1,' READ IN ',I4,
     +      ' INPUT PARAMETERS, BUT',/,
     +      15X,'EX22  VERSION ',F4.1,' CAN HOLD',
     +      ' A MAXIMUM OF',I4,'.'/)
           CALL ERROR
702    CONTINUE
C
       NHST = INT(P0(PNHST))
       DO 801 I = 1,NCC1
           P(I) = P0(PP+I-1)
801    CONTINUE
       AREA = P0(PAREA)
C
       NR   =  INT(P0(PNR))
       NORGR = INT(P0(PNORG))
       NC = NR + NORGR
       NDM = NC+NC*(NC+1)/2
C
       DO 803 I = 1,6
           ALP(I) = P0(PALP+I-1)
803    CONTINUE
       N2 = NDM+1
       DO 805 I = 1,N2
          Y(I) = C0(PY+I-1)
805    CONTINUE
       MRNO = P0(PMRNO)
       R1 = P0(PR1)
       R2 = P0(PR2)
C
       XM = P0(PXM)
       N2 = INT(P0(PNQ))
       DO 806 I = 1,N2
           Q(I) = P0(PQ+I-1)
806    CONTINUE
       CFVMX = P0(PCFVMX)
       ALINP = P0(PALINP)
       ALPAR = P0(PALPAR)
       DO 807 I=1,20
           PSTDV(I) = P0(PPSTDV+I-1)
807    CONTINUE
       ETSW = INT(P0(PETSW))
       DO 808 I=1,12
           ETDAT(I) = P0(PETDAT+I-1)
808    CONTINUE
C
       NHRSE = INT(P0(PNHRSE))
       NHRSD = INT(P0(PNHRSD))
C
C
C***********************************************************************
C
C     MRNO  :  MODEL OPERATION NUMBER - 22
C
C
C...convert internal clock times to month,day,year,hour
       CALL MDYH1 (IDA,IHR,IIMO,IIDA,IIYR,IIHR,
     +  NOUTZ,NOUTDS,TZC)
       ITM1 = 1000000*IIYR + 10000*IIMO + 100*IIDA + IIHR
       CALL MDYH1 (LDACPD,LHRCPD,IIMOF,IIDAF,IIYRF,IIHRF,
     +  NOUTZ,NOUTDS,TZC)
       ITM2 = 1000000*IIYRF + 10000*IIMOF + 100*IIDAF + IIHRF
       CALL MDYH1 (LDA,LHR,IIMOE,IIDAE,IIYRE,IIHRE,
     +  NOUTZ,NOUTDS,TZC)
       ITM3 = 1000000*IIYRE + 10000*IIMOE + 100*IIDAE + IIHRE
C
C***CASE 1: the entire run is in the past, no forecast
       IF (ITM2 .LT. ITM3) GOTO 703
           NF = 1
           NFORE = 0
C
C...figure out the number of time-steps to use for updating, returns
C... the number of interval endpoints (#intervals+1)
           CALL STEP22(IIYR,IIMO,IIDA,IIHR,
     +      IIYRE,IIMOE,IIDAE,IIHRE,NHST,NS)
           NOBSV = NS
703    CONTINUE
C
C***CASE 2: the entire run is in the future, no updating except carryove
       IF (ITM2 .GE. ITM1) GOTO 704
           NS = 1
           NOBSV = 0
C
C...figure out the number of forcase steps to use
           CALL STEP22(IIYR,IIMO,IIDA,IIHR,
     +      IIYRE,IIMOE,IIDAE,IIHRE,NHST,NF)
           NFORE = NF
704    CONTINUE
C
C***CASE 3: there is both updating and forecasting
      IF (ITM2 .LT. ITM1 .OR. ITM2 .GE.ITM3) GOTO 705
           CALL STEP22(IIYR,IIMO,IIDA,IIHR,
     +      IIYRF,IIMOF,IIDAF,IIHRF,NHST,NS)
           NOBSV = NS
           NS = NS+1

           CALL STEP22(IIYRF,IIMOF,IIDAF,IIHRF,
     +      IIYRE,IIMOE,IIDAE,IIHRE,NHST,NF)
C
C...subtract 1 for LDACPD/LHRCPD
           NF = NF-1
           NFORE = NF
705    CONTINUE
C
       IF (NF .LE. MAXNF) GOTO 706
           WRITE (IPR,934) NF,MAXNF
934        FORMAT(/10X,'**WARNING** ',I4,' FORECAST STEPS HAVE',
     +      ' BEEN REQUESTED,',/,' BUT ONLY THE PROGRAM MAXIMUM',
     +      ' OF',I4,' WILL BE DONE.'/)
           CALL WARN
           NF = MAXNF
706    CONTINUE
C
       WRITE(IPR,614)
C
       WRITE(IPR,622) (ILBL(J),J=1,15)
C
       IF(IDB .EQ. 1) WRITE(IODBUG,618)
C
C...PRINT DETAILED OUTPUT IF IDB=1
       IF(IDB .NE. 1) GOTO 707
           WRITE (IODBUG,639)
           WRITE(IODBUG,628) NHST
           WRITE(IODBUG,636) (P(IJ),IJ=10,NCC1)
           WRITE(IODBUG,638) AREA
707    CONTINUE
C
       IF (IDB .GE. 0) WRITE(IPR,619)
C
C  UPDATE - PREDICT CYCLE
C
       INDX = 1
       XHST = NHST
       M = 1
C
       N2 = NDM+1
       DO 5 I=1,N2
           YST(I)=Y(I)
5      CONTINUE
C
       VARCHN = 0.0
C
C...WRITE WMO INDICATORS
      IIMOFF = IIMOF
       IF (IIMOF .EQ. 13) IIMOFF = IIMOF - 12
       IF (IDB .GE. 0) WRITE(IPR,620) MRNO,IIMOO,IIDA,IIYR,IIHR,
     +  IIMOE,IIDAE,IIYRE,IIHRE,IIMOFF,IIDAF,IIYRF,IIHRF,
     +  NOUTZ,NOUTDS,TZC
C...4/2000-CONDITIONAL REMOVE TO ENFORCE PRINTING - IF (IBUG .GT. 0)
       WRITE (IODBUG,620) MRNO,
     +  IIMO,IIDA,IIYR,IIHR,IIMOE,IIDAE,IIYRE,IIHRE,
     +  IIMOFF,IIDAF,IIYRF,IIHRF,
     +  NOUTZ,NOUTDS,TZC
C
       IF (IDB .GE. 0) WRITE (IPR,621) NOBSV,NFORE
621    FORMAT(/10X,'NUMBER OF UPDATE   STEPS = ',I4,/,
     +         10X,'          FORECAST STEPS = ',I4)
C...4/2000-CONDITIONAL REMOVE TO ENFORCE PRINTING - IF (IBUG .GT. 0)
       WRITE (IODBUG,621) NOBSV,NFORE
C
       DIVIS = FLOAT(NHST)
C
C...figure out the offset to the start of the data in the time series ar
C... to use when addressing the data arrays with the time-step index
       IOFSET = (IDA-IDADAT)*(24/NHST) + (IHR/NHST) -1
C
C
C------------------ LOOP FOR COMPUTATIONAL PERIOD ----------------------
C
       IFIRST=1
       NCTIM=1
       KDA = IDA
       KHR = IHR
       DO 100 IST=1,NS
C
C...for this time step, get mean areal precipitation, mean areal
C... evapotranspiration, and discharge
C
C
C 4/2000-write statements shut off to create only one-line output per step
C           WRITE (IPR,9960) IST
C9960       FORMAT(/,1X,'TIME STEP= ',I4)
C
C...get discharge in CMS for previous time step, OR carryover if 1st TS
           IF (IST .GT. 1) XDISCH=DD(IOFSET+IST-1)
           IF (IST .EQ. 1) XDISCH = C0(PY+NDM)
C
           JST=IST-1
C 4/2000-write statements shut off to create only one-line output per step
C           WRITE (IPR,9966) XDISCH,JST
C9966       FORMAT('USING DISCH= ',E10.3, 'FROM TS= ',I3)
C
C...for computational steps, save observed discharge as last carryover v
C...for forecast steps, save missing time series value indicator
C          IF (IST .LT. NS) THEN
           IF (IST .LE. NOBSV) Y(NDM+1)=DD(IOFSET+IST)
           IF (IST .GT. NOBSV) Y(NDM+1) = -999.
C
C
C...check if it is a missing data value (-999.)
           IMIS = IFMSNG (XDISCH)
C...convert DISCH from CMS to mm/dt
           IF (IMIS .EQ. 0) XDISCH = XDISCH/(AREA/3.6/FLOAT(NHRSD))
           ZHD = XDISCH
C
C...get observed discharge in mm/dt for current time step
           IF (IST .GT. NOBSV) ZDD = -999.
           IF (IST .GT. NOBSV) GOTO 709
               ZDD = DD(IOFSET+IST)
               IMIS = IFMSNG(ZDD)
               IF (IMIS .EQ. 0) ZDD = ZDD/(AREA/3.6/FLOAT(NHRSD))
709        CONTINUE
C
           ZH=XDISCH
           NF1=NF-1
C
C...the last time through this loop (last IST, last UPDATE) ==>
C... use NF observed MAP & ET values to do NF disch predictions
           LF=1
           IF (IST .EQ. NS) LF=NF
C
           KHRC = KHR-NHST
           KDAC = KDA
           DO 3335 IRD=1,LF
C
C...get the calendar date for this time step
               KHRC = KHRC + NHST
               IF (KHRC-1 .LE. 23) GOTO 710
                   KHRC = MOD(KHRC-1,24)+1
                   KDAC = KDAC+1
710            CONTINUE
               CALL MDYH1 (KDAC,KHRC,MO,DA,YR,HR,
     +          NOUTZ,NOUTDS,TZC)
C
C...get mean areal precipitation (accum mm/dt) for current time step
C3335          READ(NR8,*) IDUM1,IDUM2, ZP(IRD),ZE(IRD),XDM10
               ZP(IRD) = DP(IOFSET+IST+IRD-1)
C
C...get MAPF CV factor time series for current time step
C...if datatype is wrong set all factors to 1
C...also, convert all input negative factors to 1
			 ZF(IRD)=1.0
			 IF(IST.EQ.NS) ZF(IRD) = DF(IOFSET+IST+IRD-1)
C
			 MAPCVT='DFAC'
			 READ(MAPCVT,'(A4)') ZMAPCV
			 ZTEST=P0(PDFAC)
			 IF(ABS(ZMAPCV-ZTEST).GT.1.E-10) THEN
			 WRITE(IPR, 953) NSTEP,MO,DA,YR,HR
 953		FORMAT(/10X,'**WARNING** MAP VARIANCE MODIFIERS TYPE IS INVALID',/,
     +              15X,'FOR TIME STEP#',I4,',',4X,
     +              I2,'/',I2,'/',I4,4X,I2,':00,',/,
     +              15X,'MODIFIER VALUES WILL BE',
     +              ' CHANGED TO 1.'/)
                   CALL WARN
				 ZF(IRD)=1.0
			ENDIF
			 IF(ZF(IRD).LT.0.) THEN
				WRITE (IPR, 954) NSTEP,MO,DA,YR,HR,ZF(IRD)
 954				FORMAT(/10X,'**WARNING** NEGATIVE MAP VARIANCE MODIFIER',/,
     +              15X,'FOR TIME STEP#',I4,',',4X,
     +              I2,'/',I2,'/',I4,4X,I2,':00,',/,
     +              15X,'ORIGINAL VALUE OF ',E10.3,' WILL BE',
     +              ' CHANGED TO 1.'/)
                   CALL WARN
				 ZF(IRD)=1.0
			ENDIF

C
C...do not allow precip values above a certain maximum
               IF (ZP(IRD) .LE. PMAX) GOTO 711
                   NSTEP = IST+IRD-1
                   WRITE (IPR,935) NSTEP,MO,DA,YR,HR,ZP(IRD),PMAX
935                FORMAT(/10X,'**WARNING** LARGE M.A.P. VALUE READ',/,
     +              15X,'FOR TIME STEP#',I4,',',4X,
     +              I2,'/',I2,'/',I4,4X,I2,':00,',/,
     +              15X,'ORIGINAL VALUE OF ',E10.3,' WILL BE',
     +              ' CHANGED TO ',E10.3,'.'/)
                   CALL WARN
                   ZP(IRD) = PMAX
711            CONTINUE
C
C...calculate the value for SUBS based upon the data
                SUBS = ZP(IRD)/5.0
C
C...if SUBS is not an integer value, round it up to the next integer
                IF (SUBS .NE. FLOAT(INT(SUBS)) )
     +          SUBS = FLOAT(INT(SUBS+1.0))
C...do not let it go below a minimum
               IF (SUBS .LT. 4.000) SUBS = 4.0
C
C
C...if ETSW = 1 ==> interpolate 12 monthly correction factors read in,
C...                apply result to ET time series data
C...        = 0 ==> interpolate 12 mean monthly demand values read,
C...                no ET time series used
               CALL INTR22(MO,DA,ETDAT,RESULT)
C
               IF (ETSW .EQ. 0) GOTO 712
C
C...get the most recent pot ET time series data value available
                   NET = (KDAC-IDADAT)*(24/NHRSE)+((KHRC-1)/NHRSE)+1
                   ZE(IRD) = DE(NET)
C...convert cumul. pot. to actual value using interpolated correction fa
                   ZE(IRD) = ZE(IRD)*RESULT
C...correct cumulative amount to time interval
                   ZE(IRD) = ZE(IRD) * (FLOAT(NHST)/FLOAT(NHRSE))
C
712            CONTINUE
               IF (ETSW .EQ. 1) GO TO 713
C...interpolated daily actual demand
                   ZE(IRD) = RESULT
C...correct for time interval
                   ZE(IRD) = ZE(IRD) * (FLOAT(NHST)/24.)
713            CONTINUE
C
C
C 4/2000-write statements shutoff to allow one-line output only
C           WRITE (IPR,9967) ZP(IRD), ZE(IRD), IST
C9967       FORMAT('OBS MAP,ET= ',E10.3,4X,E10.3,'FOR TS= ',I3)
C
3335       CONTINUE
C
C
C...ESTIMATOR AND INTEGRATION CONSTANTS
C
C***K.G. 880628
C          SUBS=4.
C***make SUBS an input paramter, not hardwired
C          SUBS=6.
C
           TOL=1.E-2
           TEND=1.
C
C...COMPUTE OBSERVATION ERROR VARIANCE (ASSUMES DISCHARGE INPUT IN MM/DT
           R(1)=R1+R2*XDISCH
           ZQM=XDISCH
           R(1)=R(1)*R(1)
           DO 10 IK=1,NC
               IF (YST(IK) .LE.0.) YST(IK)=0.
10         CONTINUE
           CALL CNVR22(0,NC,YST,X1,CX1)
           CALL POSD22(NC,CX1)
           CALL CNVR22(1,NC,YST,X1,CX1)
C
           IF (IMIS .EQ.0) CALL UPDT22(IBUG,IDB)
           DO 20 IJ=1,NDM
               Y(IJ)=YST(IJ)
20         CONTINUE
C
C...calculate estimated discharge (mm/dt)
           ZHEST = ALP(NR)*Y(NC)**XM
C
C...calculate estimated stdev discharge (mm/dt)
	   ZHSTD = ALP(NR)*Y(NDM)*ALP(NR)
	   ZHSTD = SQRT(ABS(ZHSTD))
	   IF (ZHSTD.LT.1.E-06)
     +         ZHSTD = 1.E-06
	   IF(IST.GT.1)
     +	       DV(IOFSET+IST-1) = ZHSTD * (AREA/3.6/FLOAT(NHRSD))
C
C...for computational time steps only, output dischg (CMS) in DS array
C... NOTE that a time step computes the estimated disch for the previous
C... time step
           IF (IST .GT. 1)
     +         DS(IOFSET+IST-1) = ZHEST * (AREA/3.6/FLOAT(NHRSD))
C
           LF=NF
C
C
C...THE FOLLOWING CAN BE CHANGED DEPENDING ON THE SPECIFICS OF THE
C... WMO INTERCOMPARISON RUNS
C
C***KG     IF (INIT.EQ.1 .AND. IST.LT.NS) LF=1
           IF (IST.LT.NS) LF=1
C
           DO 25 IK=1,NC
               IF (Y(IK) .LE. 0.) Y(IK)=0.
 25        CONTINUE
           CALL PRED22(LF,TEND,IOFSET,DS,IBUG,IDB,DV)
C
       IF (LF .NE. 1) GOTO 715
           ZPD = ZP(1)
           ZED = ZE(1)
715    CONTINUE
       IF (LF .EQ. 1) GOTO 716
           ZPD = ZP(NF)
           ZED = ZE(NF)
716    CONTINUE
C
100    CONTINUE
C
C FOLLOWING IF COMMENTED OUT 10/2000 KPG to get 13 lines IN FORECAST
C       IF (IBUG .NE. 1) GOTO 717 
           CALL MDYH1 (KDAP,KHRP,IIMOP,IIDAP,IIYRP,IIHRP,
     +      NOUTZ,NOUTDS,TZC)
CKPG 04/18/01 ADDED TO PRINT LAST FACTOR ADJ VALUE FOR LAST FORECAST
           IF(ABS(ZF(NF)-1.0).GT.1.E-6) THEN
                WRITE(IPR,*) '     MAP VARIANCE MODIFIER = ', ZF(NF)
           ENDIF
           CALL PRTD22 (IODBUG,IIMOP,IIDAP,IIYRP,IIHRP,
CKPG 12/2/97 ADDED ZSTDPD
     +      ZPD,ZED,ZHPRED,ZSTDPD,ZHD,ZHEST)
C717    CONTINUE  (COMMENTED KPG 10/2000)
C
C**************  END OF COMPUTATION LOOP  **************
C
C-----------------------------------------------------------------------
C  SAVE FINAL CARRY-OVER - REPLACE THE INITIAL CARRYOVER VALUES IN C0
C   WITH THE CARRYOVER VALUES FOR THE ENDING TIME
C
       IF (IFILLC .NE. 1) GOTO 718
           N2 = NDM+1
           DO 200 I = 1,N2
               C0(PY+I-1) = Y(I)
200        CONTINUE
718    CONTINUE
C
C***********************************************************************
C
 614     FORMAT(//////////5X,50('*')//
     *7X,'                  S S - S A C          '//
     *7X,'                  AUGUST 2002          '//
     *7X,'          HYDROLOGIC RESEARCH CENTER '/
     *7X,'         12780 HIGH BLUFF DRIVE, #250'/
     *7X,'          SAN DIEGO, CA  92130-3017  '//
     *7X,'      BASED ON HFS SOFTWARE DEVELOPED BY'/
     *7X,'          K.P.GEORGAKAKOS, FALL 1986 '/
     *14X,
     *//5X,50('*')/////)
 618     FORMAT(/////30X,'----- DETAILED OUTPUT OPTION IN EFFECT',
     *   ' -----'/////)
 619     FORMAT(/////30X,'F O R E C A S T   O U T P U T'///)
 620     FORMAT(////10X,'  FORECAST COMPONENT OPERATION ',I2,//,
     +    15X,'RUN START    ;',I2.2,'/',I2.2,'/',I4,4X,I4,/,
     +    15X,'    END      ;',I2.2,'/',I2.2,'/',I4,4X,I4,/,
     +    15X,'LAST OBS DATA;',I2.2,'/',I2.2,'/',I4,4X,I4,/,
     +    15X,'NOUTZ,NOUTDS,TZC: ',4X,I3,4X,I3,4X,A4,/)
 622     FORMAT(///5X,'RUN TITLE : ',15A4,///)
 623     FORMAT(20X,'THIS RUN PRODUCES THE INITIAL FORECAST IN A',
     *   ' FLOOD-FORECAST SEQUENCE')
 628     FORMAT(/20X,'TIME INTERVAL OF OBSERVED DATA :',I3,' HRS')
 636     FORMAT(/20X,'PARAMETER ARRAY VALUES :'/(20X,6E10.3))
 638     FORMAT(/20X,'BASIN AREA : ',F8.2,'SQ-KM')
 639     FORMAT(////30X,'U S E R   I N P U T'///)
C
       RETURN
       END
