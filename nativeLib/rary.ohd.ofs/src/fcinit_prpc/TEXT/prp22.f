C MODULE PRP22
C
       SUBROUTINE PRP22(P0)
       REAL P0(1)
C
C***********************************************************************
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
C...sim discharge stdev time-series internal identifier name
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
C...sim discharge stdev time-series data type  code
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
C
C***********************************************************************
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
C***********************************************************************
C
       REAL*4 SNAME(2)
       INTEGER NOPER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp22.f,v $
     . $',                                                             '
     .$Id: prp22.f,v 1.3 2002/05/15 13:54:35 hank Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
       DATA SNAME/4HPRP2,4H2   /
       DATA NOPER/22/
C
C***********************************************************************
C
       CALL FPRBUG (SNAME,1,NOPER,IBUF)
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
       IVAL = INT(P0(PMRNO))
       WRITE (IPR,8) IVAL
8      FORMAT(///10X,'INPUT PARAMETERS & INITIAL CONDITIONS',
     +  ' FOR OPERATION ',I2)
C
       WRITE (IPR,10) P0(POPVER),(P0(PILBL+I-1),I=1,15)
10     FORMAT(/,10X,'STATE-SPACE SAC SMA (SS-SAC), VERSION ',F3.1,
     +  ', FOR: ',/,15X,15A4,/)
C
       IVAL = INT(P0(PNORG))
       WRITE (IPR,112) IVAL
112    FORMAT(/,10X,'NUMBER OF STATES OF SACREMENTO',
     +  ' MODEL = ',I4)
C
       IVAL = INT(P0(PNHST))
       WRITE (IPR,121) IVAL
121    FORMAT(/,10X,'COMPUTATIONAL TIME INTERVAL IS ',I2,' HOURS')
C
       WRITE (IPR,1211)
1211   FORMAT(/10X,'NUMBER OF INITIAL SUBDIVISIONS',
     +  ' FOR INTEGRATION = ',/,
     +  15X,'(time step M.A.P. value)/5.0')
C
       WRITE (IPR,12)
12     FORMAT(/,10X,'TIME SERIES BEING USED BY THIS OPERATION:',/)
C
       WRITE (IPR,13)
13     FORMAT(15X,'CONTENTS',T45,'I.D.',T60,'TYPE',T70,
     +  'TIME INT',/)
C
       TSP(1) = P0(PTSP)
       TSP(2) = P0(PTSP+1)
       IVAL = INT(P0(PNHST))
       WRITE (IPR,14) (P0(PTSP+I-1),I=1,2),P0(PDTCP),IVAL
14     FORMAT(15X,'MEAN AREAL PRECIPITATION ',T45,2A4,T60,A4,T70,
     +  I2,' HOURS')
       IVAL = INT(P0(PNHRSE))
       IF (INT(P0(PETSW)) .EQ. 1)
     +  WRITE (IPR,15) (P0(PTSE+I-1),I=1,2),P0(PDTCE),IVAL
15     FORMAT(15X,'MEAN AREAL EVAPOTRANSP ',T45,2A4,T60,A4,
     +  T70,I2,' HOURS')
       IVAL = INT(P0(PNHRSD))
       WRITE (IPR,16) (P0(PTSD+I-1),I=1,2),P0(PDTCD),IVAL
16     FORMAT(15X,'DISCHARGE',T45,2A4,T60,A4,T70,I2,' HOURS')
       IVAL = INT(P0(PNHRSD))
       WRITE (IPR,161) (P0(PTSS+I-1),I=1,2),P0(PDTCS),IVAL
161    FORMAT(15X,'SIMULATED DISCHARGE',
     +  T45,2A4,T60,A4,T70,I2,' HOURS')
       IVAL = INT(P0(PNHRSD))
       WRITE (IPR,162) (P0(PTSSD+I-1),I=1,2),P0(PDTCSD),IVAL
162    FORMAT(15X,'SIMULATED DISCHARGE STD', 
     +  T45,2A4,T60,A4,T70,I2,' HOURS')
       WRITE (IPR,163) (P0(PTSFA+I-1),I=1,2),P0(PDFAC),IVAL
163    FORMAT(15X,'MAP VARIANCE MODIFIERS', 
     +  T45,2A4,T60,A4,T70,I2,' HOURS')
C
C
       WRITE (IPR,30)
30     FORMAT(/,10X,'PARAMETER VALUES (basin dependent):')
C
       IF (INT(P0(PETSW)) .EQ. 1)
     +  WRITE (IPR,51) (P0(PETDAT+I-1),I=1,12)
51      FORMAT(/,13X,'PEADJ - POTENTIAL ET 16th OF MONTH',
     +   ' 24 hr CORRECTION FACTORS',
     +   ' (JAN-DEC):',/,5(15X,6(E10.3),/))
       IF (INT(P0(PETSW)) .EQ. 1)
     +  WRITE (IPR,52) (P0(PETDAT+I-1),I=1,12)
52      FORMAT(/,13X,'ET MONTHLY MEAN DEMANDS',
     +   ' (JAN-DEC):',/,5(15X,6(E10.3),/))
C
       WRITE (IPR,31)
31     FORMAT(/,13X,'MODIFIED-SACREMENTO MODEL SOIL PARAMETERS',/)
C
C...1st parameter is always 1.0
C...parameters 2,3, & 4:
       WRITE (IPR,310) P0(PP+INT(P0(PSP))+2-2),P0(PP+INT(P0(PSP))+3-2),
     +  P0(PP+INT(P0(PSP))+4-2)
310    FORMAT(15X,'UZTWM = ',T25,E10.3,T37,'UZFWM =',T47,E10.3,
     +  T59,'LZTWM =',T69,E10.3)
C...parameters 5,6, & 7:
       WRITE (IPR,311) P0(PP+INT(P0(PSP))+5-2),P0(PP+INT(P0(PSP))+6-2),
     +  P0(PP+INT(P0(PSP))+7-2)
311    FORMAT(15X,'LZFPM = ',T25,E10.3,T37,'LZFSM =',T47,E10.3,
     +  T59,'UZK =',T69,E10.3)
C...parameters 8,9, & 10:
       WRITE (IPR,312) P0(PP+INT(P0(PSP))+8-2),P0(PP+INT(P0(PSP))+9-2),
     +  P0(PP+INT(P0(PSP))+10-2)
312    FORMAT(15X,'LZPK = ',T25,E10.3,T37,'LZSK =',T47,E10.3,
     +  T59,'ZPERC =',T69,E10.3)
C...parameters 11,12, & 13:
       WRITE (IPR,313) P0(PP+INT(P0(PSP))+11-2),
     +  P0(PP+INT(P0(PSP))+12-2),
     +  P0(PP+INT(P0(PSP))+13-2)
313    FORMAT(15X,'REXP = ',T25,E10.3,T37,'PFREE =',T47,E10.3,
     +  T59,'SIDE =',T69,E10.3)
C...parameters 14 & 15:
       WRITE (IPR,314) P0(PP+INT(P0(PSP))+14-2),P0(PP+INT(P0(PSP))+15-2)
314    FORMAT(15X,'ADIMP = ',T25,E10.3,T37,'PCTIM =',T47,E10.3,/)
C...parameters 16,17 & 18 are hardwired = 8
C
       WRITE (IPR,33) P0(PAREA)
33     FORMAT(/,13X,'CATCHMENT AREA (SQKM) = ',E10.3)
C
       IVAL = INT(P0(PNR))
       WRITE (IPR,34) IVAL
34     FORMAT(/,13X,'NUMBER OF ROUTING MODEL RESERVOIRS = ',I2)
C
       WRITE (IPR,35) P0(PXM), P0(PALPD)
35     FORMAT(/,13X,'EXPONENT & COEFFICIENT PARAMETERS',/,
     +          13X,' OF GENERIC NON-LINEAR ROUTING RESERVOIR = ',/,
     +    15X,E10.3,' , ',E10.3)
C
       WRITE (IPR,351)
351    FORMAT(/13X,'VARIANCE PARAMETERS OF OBSERVED DATA:',/)
C
       WRITE (IPR,371)
371    FORMAT(/,15X,'DATA',T30,'CONSTANT',T50,
     +  'COEF OF VARIATION',/,T30,'STANDARD DEVIATION',/)
C
       WRITE (IPR,36) P0(PP+INT(P0(PPCSD))-1),
     +  P0(PP+INT(P0(PPCV))-1)
36     FORMAT(17X,'M.A.P.',T30,E10.3,T50,E10.3)
C
       WRITE (IPR,37) P0(PP+INT(P0(PECSD))-1),
     +  P0(PP+INT(P0(PECV))-1)
37     FORMAT(17X,'EVAPOTRANSP',T30,E10.3,T50,E10.3)
C
       WRITE (IPR,38) P0(PR1), P0(PR2)
38     FORMAT(17X,'DISCHARGE',T30,E10.3,T50,E10.3)
C
       WRITE (IPR,41) P0(PALINP)
41     FORMAT(/,13X,'COEF OF INPUT COMPONENT',
     +  ' OF MODEL-ERROR COVARIANCE = ',T70,E10.3)
C
       WRITE (IPR,412) P0(PALPAR)
412    FORMAT(/,13X,'COEF OF PARAMETER COMPONENT',
     +  ' OF MODEL-ERROR COVARIANCE = ',T70,E10.3)
C
       WRITE (IPR,42)
42     FORMAT(/,13X,'STANDARD DEVIATION OF RAINFALL-RUNOFF',/,13X,
     +  ' MODEL PARAMETER ESTIMATES:',/)
       WRITE (IPR,420) P0(PPSTDV+1-1),P0(PPSTDV+2-1),P0(PPSTDV+3-1)
420    FORMAT(15X,'UZTWM =',T25,E10.3,T37,'UZFWM =',T47,E10.3,
     +  T59,'LZTWM = ',T69,E10.3)
       WRITE (IPR,421) P0(PPSTDV+4-1),P0(PPSTDV+5-1),P0(PPSTDV+6-1)
421    FORMAT(15X,'LZFPM =',T25,E10.3,T37,'LZFSM =',T47,E10.3,
     +  T59,'UZK = ',T69,E10.3)
       WRITE (IPR,422) P0(PPSTDV+7-1),P0(PPSTDV+8-1),P0(PPSTDV+9-1)
422    FORMAT(15X,'LZPK =',T25,E10.3,T37,'LZSK =',T47,E10.3,
     +  T59,'ZPERC = ',T69,E10.3)
       WRITE (IPR,423) P0(PPSTDV+10-1),P0(PPSTDV+11-1),P0(PPSTDV+12-1)
423    FORMAT(15X,'REXP =',T25,E10.3,T37,'PFREE =',T47,E10.3,
     +  T59,'SIDE = ',T69,E10.3)
       WRITE (IPR,424) P0(PPSTDV+13-1),P0(PPSTDV+14-1),P0(PPSTDV+15-1)
424    FORMAT(15X,'ADIMP =',T25,E10.3,T37,'PCTIM =',T47,E10.3,
     +  T59,'ALPD = ',T69,E10.3)
       WRITE (IPR,425) P0(PPSTDV+16-1)
425    FORMAT(15X,'EXP =',T25,E10.3,/)
C
C
       WRITE (IPR,43)
43     FORMAT(/,13X,'MODIFIED-SACRAMENTO MODEL',
     +  ' INITIAL SOIL CONDITIONS: ',/)
       WRITE (IPR,430) P0(PSCM+1-1),P0(PSCM+2-1),P0(PSCM+3-1)
430    FORMAT(15X,'UZTWC =',T25,E10.3,T37,'UZFWC =',T47,E10.3,
     +  T59,'LZTWC =',T69,E10.3)
       WRITE (IPR,431) P0(PSCM+4-1),P0(PSCM+5-1),P0(PSCM+6-1)
431    FORMAT(15X,'LZFPC =',T25,E10.3,T37,'LZFSC =',T47,E10.3,
     +  T59,'ADIMC =',T69,E10.3)
C
C
       I6 = 6+INT(P0(PNR))
       WRITE (IPR,44) (P0(PSCMCV+I-1),I=1,I6)
44     FORMAT(/,13X,'DIAGONAL ELEMENTS OF INITIAL STATE',
     +  ' COVARIANCE MATRIX: ',/,
     +  5(15X,6(E10.3),/))
C
       WRITE (IPR,441) P0(PXDIS1)
441    FORMAT(/10X,'INITIAL DISCHARGE TIME-SERIES',
     +  ' VALUE USED = ',E10.3)
C
       IF (ITRACE .GT. 0) WRITE (IODBUG,990)
990    FORMAT(/10X,'** EXIT PRP22.')
C
       RETURN
       END
