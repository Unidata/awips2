C     MODULE TAB22
C
      SUBROUTINE TAB22(T,LEFT,IUSET,NXT,LP,P,LC,TS,MTS)
C............................................
C
C    THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE (SS-SAC)
C    STATE-SPACE SACRAMENTO SMA AND NONLINEAR ROUTING OPERATION.
C............................................
C
C    SUBROUTINE ORIGINALLY WRITTEN BY GEORGE F. SMITH - HRL - SEPT 1988.
C............................................
C
C POINTERS TO THINGS NEEDED FROM SS-SAC PART OF P ARRAY.
C!!!BE SURE TO RE-COMPILE THIS FILE, ALONG WITH OTHERS USING
C    THIS INCLUDE FILE, IF IT CHANGES.
C
C
C*************** VARIABLES STORED IN PARAMETER & CARRYOVER ARRAYS ******
C...parameters and pointers to their storage location in P0 & C0 arrays
C
C THIS SCHEME
C    DECLARES A POINTER VARIABLE FOR EVERY INPUT PARAMETER AND CARRYOVER
C     VARIABLE FOR THE SS-SAC OPERATIONN.  THE CONTENTS OF EACH POINTER
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
C...simulated stdev discharge time-series internal identifier name
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
C...simulated discharge time-series data type code
       REAL DTCS
       INTEGER PDTCS
C
C...simulated stdev discharge time-series data type code
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
C............................................
C
      INTEGER T
      DIMENSION P(1),T(1),TS(MTS)
      DIMENSION SUBNAM(2)
C
C    COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab22.f,v $
     . $',                                                             '
     .$Id: tab22.f,v 1.3 2002/05/15 13:40:59 hank Exp $
     . $' /
C    ===================================================================
C
C
C    DATA STATEMENTS
      DATA SUBNAM/4HTAB2,4H2   /
C............................................
C
C    TRACE LEVEL IS 1 - CHECK IF DEBUG IS ON
C
      CALL FPRBUG(SUBNAM,1,22,IBUG)
C............................................
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
C    IS THERE ENOUGH SPACE IN T ARRAY?
      LENGTH=10
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0)GO TO 10
C
C    NOT ENOUGH SPACE - SET IUSET AND RETURN
C
      IUSET=0
      GO TO 999
C
C    IS ENOUGH SPACE - FILL T ARRAY
 10   T(1)=22
      T(2)=NXT+LENGTH
      T(3)=LP
      T(4)=LC
C
C    FIND LOCATION OF RAIN+MELT TIME SERIES AND STORE IN T
C
C      IDT=P(54)
      IDT = P(PNHST)
C      CALL CKINPT(P(45),P(51),IDT,LD,TS,MTS,IERR)
      CALL CKINPT(P(PTSP),P(PDTCP),IDT,LD,TS,MTS,IERR)
      T(5)=LD
C
C    FIND LOCATION OF EVAPOTRANSPIRATION TIME SERIES AND STORE IN T
C
C      IDT=P(2024)
      IDT = P(PNHRSE)
C      CALL CKINPT(P(49),P(53),IDT,LD,TS,MTS,IERR)
      CALL CKINPT(P(PTSE),P(PDTCE),IDT,LD,TS,MTS,IERR)
      T(6)=LD
C
C    FIND LOCATION OF OBSERVED DISCHARGE TIME SERIES AND STORE IN T
C
C      IDT=P(2025)
      IDT = P(PNHRSD)
C      CALL CKINPT(P(47),P(52),IDT,LD,TS,MTS,IERR)
      CALL CKINPT(P(PTSD),P(PDTCD),IDT,LD,TS,MTS,IERR)
      T(7)=LD
C
C    FIND LOCATION OF SIMULATED DISCHARGE TIME SERIES AND STORE IN T
C
C      IDT=P(54)
      IDT = P(PNHST)
C      CALL FINDTS(P(2027),P(2029),IDT,LD,LTS,DIM)
      CALL FINDTS(P(PTSS),P(PDTCS),IDT,LD,LTS,DIM)
      T(8)=LD
C
C    IF SIMULATED DISCHARGE TIME SERIES IS FOUND - MARK TS ARRAY
C      TO INDICATE THAT IT IS WRITTEN TO
C
      IF (LTS.GT.0) TS(LTS+8)=1.01
C
C    FIND LOC OF SIMULATED DISCHARGE STDEV TS AND STORE IN T
C
      IDT = P(PNHST)
      CALL FINDTS(P(PTSSD),P(PDTCSD),IDT,LD,LTS,DIM)
      T(9)=LD
C
C    IF SIMULATED STDEV DISCHARGE TS IS FOUND - MARK TS ARRAY
C      TO INDICATE THAT IT IS WRITTEN TO
C
      IF (LTS.GT.0) TS(LTS+8)=1.01

C    FIND LOC OF COVARIANCE FACTORS TS AND STORE IN T
C
      IDT = P(PNHST)
C      CALL FINDTS(P(PTSFA),P(PDFAC),IDT,LD,LTS,DIM)
      CALL CKINPT(P(PTSFA),P(PDFAC),IDT,LD,TS,MTS,IERR)
      T(10)=LD
C
C............................................
C
      IUSET=LENGTH
C
C    ALL ENTRIES HAVE BEEN MADE.
C............................................
C
C    DEBUG OUTPUT
      IF(IBUG.GT.0)WRITE(IODBUG,900)(T(I),I=1,LENGTH)
 900  FORMAT('0CONTENTS OF T ARRAY FOR SS-SAC OPERATION=',10I6)
C............................................
C
C    THE T ARRAY ENTRIES FOR THE SS-SAC OPERATION ARE AS FOLLOWS.
C
C       POSITION                   CONTENTS
C          1.        I.D. NUMBER FOR THE OPERATION (=22)
C          2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY
C          3.        LOCATION OF PARAMETERS IN THE P ARRAY = LP
C          4.        LOCATION OF CARRYOVER IN THE C ARRAY = LC
C          5.        LOCATION OF RAIN+MELT DATA IN THE D ARRAY
C          6.        LOCATION OF EVAPOTRANSPIRATION DATA IN THE D ARRAY
C          7.        LOCATION OF OBSERVED DISCHARGE DATA IN THE D ARRAY
C          8.        LOCATION OF SIMULATED DISCHARGE DATA IN THE D ARRAY
C          9.        LOCATION OF SIM DISHCH STDEV DATA IN THE D ARRAY
C         10.        LOCATION OF COVARIANCE FACTORS DATA IN THE D ARRAY
C............................................
C
 999  RETURN
      END
