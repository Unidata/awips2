C MODULE COX22
C
       SUBROUTINE COX22(POLD,COLD,P0NEW,C0NEW)
       REAL POLD(1), COLD(1), P0NEW(1), C0NEW(1)
C
C***********************************************************************
C  PARAMS.INC
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
       REAL SNAME(2)
       INTEGER NOPER
       REAL TARGET
       REAL*8 STRNG1, STRNG2, STRNG3
C
       REAL*8 PNAME(16)
       REAL*8 CNAME(6)
       REAL*4 MON(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox22.f,v $
     . $',                                                             '
     .$Id: cox22.f,v 1.3 2002/05/15 17:28:32 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
       DATA SNAME/4HCOX2,4H2   /
       DATA NOPER/22/
       DATA PNAME/8HUZTWM   ,8HUZFWM   ,8HLZTWM   ,8HLZFPM   ,
     +            8HLZFSM   ,8HUZK     ,8HLZPK    ,8HLZSK    ,
     +            8HZPERC   ,8HREXP    ,8HPFREE   ,8HSIDE    ,
     +            8HADIMP   ,8HPCTIM   ,8HALPD    ,8HEXP     /
       DATA CNAME/8HUZTWC   ,8HUZFWC   ,8HLZTWC   ,8HLZFPC   ,
     +            8HLZFSC   ,8HADIMC   /
       DATA MON/4HJAN ,4HFEB ,4HMAR ,4HAPR ,4HMAY ,4HJUN ,4HJUL ,
     +          4HAUG ,4HSEP ,4HOCT ,4HNOV ,4HDEC /
       DATA STRNG1/8HUZK     /
       DATA STRNG2/8HLZPK    /
       DATA STRNG3/8HLZSK    /
C
C***********************************************************************
C
       CALL FPRBUG (SNAME,1,NOPER,IBUF)
C
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
C
C...check for change in program version, which may indicate that
C... previous carryover cannot be used, etc.
       IF (POLD(POPVER) .EQ. P0NEW(POPVER)) GOTO 801
           WRITE (IPR,10) POLD(POPVER),P0NEW(POPVER)
10         FORMAT(/10X,'**WARNING** OPERATION VERSION ',
     +      ' CHANGED',/,15X,'FROM ',F3.1,' TO ',F3.1)
           CALL WARN
801    CONTINUE
C
C...check for title change which may indicate parameters are for a
C... different basin, etc.
       ICHGE=0
       DO 120 I = 1,15
           IF (POLD(PILBL+I-1) .NE. P0NEW(PILBL+I-1) ) ICHGE=1
120    CONTINUE
       IF (ICHGE .NE. 1) GOTO 802
           WRITE (IPR,101) (POLD(PILBL+I-1),I=1,15),
     +      (P0NEW(PILBL+I-1),I=1,15)
101        FORMAT(/10X,'PARAMETER TITLE HAS CHANGED,',/,
     +         15X,'FROM:',20X,15A4,/,15X,'TO:',20X,15A4)
           CALL WARN
802    CONTINUE
C
C
C***** CHECK COMPUTATIONAL INTERVAL & PARAMETERS DEPENDENT UPON IT *****
C
       INTOLD = INT(POLD(PNHST))
       INTNEW = INT(P0NEW(PNHST))
       IF (INTOLD .EQ. INTNEW) GOTO 803

       WRITE (IPR,121) INTOLD, INTNEW
121    FORMAT(/10X,'**WARNING** COMPUTATIONAL TIME INTERVAL',
     +  ' CHANGED',/,15X,' FROM ',I2,' TO ',I2,' HOURS')
       CALL WARN
C
C...check if time-interval dependent parameters UZK, LZPK, & LZSK were
C... modified to reflect the change in time-interval
C
       RATIO  = FLOAT(INTOLD)/FLOAT(INTNEW)
C
C...check 7th soil parameter in P - UZK
       VALOLD = POLD(PP+INT(POLD(PSP))+7-2)
       VALNEW = P0NEW(PP+INT(P0NEW(PSP))+7-2)
       TARGET = VALOLD/RATIO
C
       IF (VALNEW .EQ. TARGET ) GOTO 804
           WRITE (IPR,125) STRNG1,VALOLD,VALNEW,TARGET
125        FORMAT(/10X,'**WARNING** ',A8,' soil parameter',
     +      ' CHANGED',/,15X,' FROM ',E10.3,' TO ',E10.3,/,15X,
     +      ' BUT SHOULD CHANGE TO ',E10.3)
           CALL WARN
804    CONTINUE
C
C...check 8th soil parameter - LZPK
       VALOLD = POLD(PP+INT(POLD(PSP))+8-2)
       VALNEW = P0NEW(PP+INT(P0NEW(PSP))+8-2)
       TARGET = VALOLD/RATIO
C
       IF (VALNEW .EQ. TARGET ) GOTO 805
           WRITE (IPR,125) STRNG2,VALOLD,VALNEW,TARGET
           CALL WARN
805    CONTINUE
C
C...check 9th soil parameter - LZSK
       VALOLD = POLD(PP+INT(POLD(PSP))+9-2)
       VALNEW = P0NEW(PP+INT(P0NEW(PSP))+9-2)
       TARGET = VALOLD/RATIO
C
       IF (VALNEW .EQ. TARGET ) GOTO 806
           WRITE (IPR,125) STRNG3,VALOLD,VALNEW,TARGET
           CALL WARN
806    CONTINUE
C
803    CONTINUE
C
C
C*** CHECK TIME-SERIES INTERNAL IDENTIFIERS/DATA TYPE-CODES ************
C
       IF (POLD(PTSP).EQ.P0NEW(PTSP) .AND.
     +     POLD(PTSP+1) .EQ. P0NEW(PTSP+1) ) GOTO 807
       WRITE (IPR,14) (POLD(PTSP+I-1),I=1,2),(P0NEW(PTSP+I-1),I=1,2)
14     FORMAT(/10X,'**WARNING** MEAN AREAL PRECIPITATION INTERNAL',
     +  ' IDENTIFIER CHANGED',/,15X,' FROM ',2A4,' TO ',2A4)
       CALL WARN
807    CONTINUE
C
       IF (POLD(PDTCP) .EQ. P0NEW(PDTCP)) GOTO 808
           WRITE (IPR,141) POLD(PDTCP), P0NEW(PDTCP)
141        FORMAT(/10X,'**WARNING** MEAN AREAL PRECIPITATION DATA',
     +      ' TYPE-CODE CHANGED',/,15X,' FROM ',A4,' TO ',A4)
           CALL WARN
808    CONTINUE
C
       IF (POLD(PTSD)   .EQ. P0NEW(PTSD) .AND.
     +     POLD(PTSD+1) .EQ. P0NEW(PTSD+1) ) GOTO 809
       WRITE (IPR,142) (POLD(PTSD+I-1),I=1,2),(P0NEW(PTSD+I-1),I=1,2)
142    FORMAT(/10X,'**WARNING** DISCHARGE INTERNAL IDENTIFIER',
     +  ' CHANGED',/,15X,' FROM ',2A4,' TO ',2A4)
       CALL WARN
809    CONTINUE
C
       IF (POLD(PDTCD) .EQ. P0NEW(PDTCD)) GOTO 810
       WRITE (IPR,143) POLD(PDTCD),P0NEW(PDTCD)
143    FORMAT(/10X,'**WARNING** DISCHARGE DATA TYPE-CODE CHANGED',/,
     + 15X,' FROM ',A4,' TO ',A4)
       CALL WARN
810    CONTINUE
C
       IF (POLD(PTSS)   .EQ. P0NEW(PTSS) .AND.
     +     POLD(PTSS+1) .EQ. P0NEW(PTSS+1) ) GOTO 811
       WRITE (IPR,144) (POLD(PTSS+I-1),I=1,2),(P0NEW(PTSS+I-1),I=1,2)
144    FORMAT(/10X,'**WARNING** SIMULATED DISCHARGE INTERNAL',
     +  ' IDENTIFIER CHANGED',/,15X,' FROM ',2A4,' TO ',2A4)
       CALL WARN
811    CONTINUE
C
       IF (POLD(PTSSD)  .EQ. P0NEW(PTSSD) .AND.
     +     POLD(PTSSD+1) .EQ. P0NEW(PTSSD+1) ) GOTO 8112
       WRITE(IPR,1442) (POLD(PTSSD+I-1),I=1,2),
     +     (P0NEW(PTSSD+I-1),I=1,2)
1442   FORMAT(/10X,'**WARNING** SIM DISCHARGE STDEV INTERNAL',
     +  ' IDENTIFIER CHANGED',/,15X,' FROM ',2A4,' TO ',2A4)
       CALL WARN
8112   CONTINUE
C
       IF (POLD(PDTCS) .EQ. P0NEW(PDTCS) ) GOTO 812
       WRITE (IPR,145) POLD(PDTCS), P0NEW(PDTCS)
145    FORMAT(/10X,'**WARNING** SIMULATED DISCHARGE DATA TYPE-CODE',
     +  ' CHANGED',/,15X,' FROM ',A4,' TO ',A4)
       CALL WARN
812    CONTINUE
C
       IF (POLD(PDTCSD) .EQ. P0NEW(PDTCSD) ) GOTO 8122
       WRITE (IPR,1452) POLD(PDTCSD), P0NEW(PDTCSD)
1452   FORMAT(/10X,'**WARNING** SIM DISCHARGE STDEV DATA TYPE-CODE',
     +  ' CHANGED',/,15X,' FROM ',A4,' TO ',A4)
       CALL WARN
8122   CONTINUE
C
C*** CHECKS ON EVAPOTRANSPIRATION PARAMETERS ***************************
C
       IOLD = INT(POLD(PETSW))
       INEW = INT(P0NEW(PETSW))
C
C...if still using 12 monthly actual ET  demand values
       IF (IOLD.NE.0 .OR. INEW.NE.0) GOTO 813
           DO 201 I=1,12
               VALOLD = POLD(PETDAT+I-1)
               VALNEW = P0NEW(PETDAT+I-1)
               IF (VALOLD .EQ. VALNEW) GOTO 814
                   WRITE (IPR,156) MON(I),VALOLD,VALNEW
156                FORMAT(/10X,'**WARNING** ET DEMAND VALUE',
     +              ' FOR ',A4,'CHANGED',/,15X,'FROM ',
     +              F8.3,' TO ',F8.3)
                   CALL WARN
814            CONTINUE
201        CONTINUE
813    CONTINUE
C
C...if still using potential ET time-series & 12 correction factors
       IF (IOLD.NE.1 .OR. INEW.NE.1) GOTO 815
C
       IF (POLD(PTSE).EQ.P0NEW(PTSE) .AND.
     +     POLD(PTSE+1).EQ.P0NEW(PTSE+1) ) GOTO 816
       WRITE (IPR,15) (POLD(PTSE+I-1),I=1,2),(P0NEW(PTSE+I-1),I=1,2)
15     FORMAT(/10X,'**WARNING** MEAN AEREAL EVAPOTRANSP INTERNAL',
     +  ' IDENTIFIER CHANGED',/,15X,' FROM ',2A4,' TO ',2A4)
       CALL WARN
816    CONTINUE
C
       IF (POLD(PDTCE) .EQ. P0NEW(PDTCE)) GOTO 817
       WRITE (IPR,151) POLD(PDTCE), P0NEW(PDTCE)
151    FORMAT(/10X,'**WARNING** MEAN AEREAL EVAPOTRANSP DATA',
     +  ' TYPE-CODE CHANGED',/,15X,' FROM ',A4,' TO ',A4)
       CALL WARN
817    CONTINUE
C
       IF (POLD(PNHRSE) .EQ. P0NEW(PNHRSE) ) GOTO 818
       WRITE (IPR,152) POLD(PNHRSE), P0NEW(PNHRSE)
152    FORMAT(/10X,'**WARNING** MEAN AEREAL EVAPOTRANSP DATA',
     +  ' INTERVAL CHANGED',/,15X,' FROM ',I2,' TO ',I2)
       CALL WARN
818    CONTINUE
C
       DO 200 I=1,12
           VALOLD = POLD(PETDAT+I-1)
           VALNEW = P0NEW(PETDAT+I-1)
           IF (VALOLD .EQ. VALNEW) GOTO 819
               WRITE (IPR,154) MON(I),VALOLD,VALNEW
154            FORMAT(/10X,'**WARNING** ET CORRECTION FACTOR',
     +          ' FOR ',A4,'CHANGED',/,15X,'FROM ',F8.3,
     +          ' TO ',F8.3)
               CALL WARN
819         CONTINUE
C
200    CONTINUE
C
815    CONTINUE
C
C...if changing from actual ET demand values to potential ET time series
       IF (IOLD.NE.0 .OR. INEW.NE.1) GOTO 820
           WRITE (IPR,1571)
1571       FORMAT(/10X,'**WARNING** CHANGING OVER FROM USE OF',
     +      ' ACTUAL TO POTENTIAL ET DATA.')
           CALL WARN
C
           IWARN = 0
           DO 202 I=1,12
               VALOLD = POLD(PETDAT+I-1)
               VALNEW = P0NEW(PETDAT+I-1)
               IF (VALOLD .NE. VALNEW) GOTO 821
                   WRITE (IPR,157) MON(I),VALOLD
157                FORMAT(/10X,'**WARNING** MONTHLY ET FACTOR',
     +              ' FOR ',A4,'DID NOT CHANGE FROM: ',F8.3)
                   CALL WARN
821            CONTINUE
202        CONTINUE
820    CONTINUE
C
C...changing from use of potential to actual ET data
       IF (IOLD.NE.1 .OR. INEW.NE.0) GOTO 822
            WRITE (IPR,158)
158         FORMAT(/10X,'**WARNING** CHANGING OVER FROM USE',
     +       ' OF POTENIAL TO ACTUAL ET DATA.')
            CALL WARN
C
            DO 203 I =1,12
                VALOLD = POLD(PETDAT+I-1)
                VALNEW = P0NEW(PETDAT+I-1)
                IF (VALOLD .NE. VALNEW) GOTO 823
                    WRITE (IPR,157) I,VALOLD
                    CALL WARN
823             CONTINUE
203          CONTINUE
822     CONTINUE
C
C************* CHECK MODEL SOIL PARAMETERS ****************************
C
C...1st parameter is always 1.0, 16-18 are always 8.0
       DO 250 I=2,15
           VALOLD = POLD(PP+INT(POLD(PSP))+I-2)
           VALNEW = P0NEW(PP+INT(P0NEW(PSP))+I-2)
           IF (VALOLD .EQ. VALNEW) GOTO 825
               WRITE (IPR,160) PNAME(I-1),VALOLD,VALNEW
160            FORMAT(/10X,'**WARNING** SOIL PARAMETER ',A8,
     +          ' HAS CHANGED',/,15X,' FROM ',E10.3,' TO ',E10.3)
               CALL WARN
825        CONTINUE
250    CONTINUE
C
C
C**** ADJUST STATE MEAN CARRYOVER VALUES AGAINST NEW CAPACITY PARAMETERS
C...copy result to 1st 6 values in new carryover array C0NEW
C
C...new UZTWM parameter against current UZTWC state
C...    UZFWM "                       " UZFWC
C...    LZTWM "                       " LZTWC
C...    LZFPM "                       " LZFPC
C...    LZFSM "                       " LZFSC
C...    LZTWM "                       " ADIMC
C
       DO 260 J=1,6
           IF (J .LT. 6) I = J+1
           IF (J .EQ. 6) I = 4
C
           CO = COLD(PY+J-1)
           PO = POLD(PP+INT(POLD(PSP))+I-2)
           PN = P0NEW(PP+INT(P0NEW(PSP))+I-2)
           DEFICT = (PO-CO)/PO
           CN = (1.0-DEFICT)*PN
           C0NEW(PY+J-1) = CN
260    CONTINUE
C
C
C******* CHECK FOR CHANGES IN CATCHMENT AREA & ADJUST CARRYOVER ********
C IF THE AREA CHANGES, THEN ENTIRE STATE ARRAY (MEANS, VARIANCES,
C AND COVARIANCES WILL BE ADJUSTED AND COPIED TO C0NEW)
C
       AOLD = POLD(PAREA)
       ANEW = P0NEW(PAREA)
       IAREA = 0
       IF (AOLD .EQ. ANEW) GOTO 826
           IAREA = 1
           RATIO = AOLD/ANEW
826    CONTINUE
C
       NROLD = INT(POLD(PNR))
       NRNEW = INT(P0NEW(PNR))
       NCOLD = 6 + NROLD
       NCNEW = 6 + NRNEW
       INR = 0
       IF (NROLD .EQ. NRNEW) INR = 1
C
       IF (IAREA .NE. 1) GOTO 827
           WRITE (IPR,33) AOLD,ANEW
33         FORMAT(/10X,'**WARNING** CATCHMENT AREA CHANGED',/,15X,
     +      'FROM ',E10.3,' TO ',E10.3,' (SQKM)')
           CALL WARN
827    CONTINUE
C
       IF (INR .NE. 1) GOTO 828
           WRITE (IPR,331) NROLD,NRNEW
331        FORMAT(/10X,'**WARNING** NUMBER OF ROUTER PARAMETERS',
     +      ' CHANGED',/,15X,'FROM ',I2,' TO ',I2)
           CALL WARN
828    CONTINUE
C
       IF (IAREA.EQ.1 .AND. INR.EQ.0) WRITE(IPR,3321)
3321   FORMAT(10X,'ALL CARRYOVER ADJUSTED ACCORDINGLY.')
C
       IF (IAREA.EQ.1 .AND. INR.EQ.1) WRITE(IPR,333)
333    FORMAT(10X,'ONLY SOIL CARRYOVER ADJUSTED.')
C
       IF (IAREA.EQ.0 .AND. INR.EQ.1) WRITE(IPR,3341)
3341   FORMAT(10X,'CURRENT SOIL CARRYOVER RETAINED,',/,10X,
     +             'ROUTER CARRYOVER SET TO INITIAL CONDITIONS.')
C
C...adjust all SOIL carryover
       IF (IAREA .NE. 1) GOTO 829
C
C...NR changed, adjust only soil carryover, ignore rest of carryover
C... (routing) vector & covariance matrix
           IF (INR .EQ. 1) I6 = 6
C...NR unchanged, adjust router state means & covariances also
           IF (INR .NE. 1) I6 = 6 + NROLD
C
C...adjust state means, either soil only OR soil+router
           DO 500 I = 1,I6
C...use soil means that have been adjusted already for deficit
               IF (I .LE. 6) VALOLD = C0NEW(I)
               IF (I .GT. 6) VALOLD = COLD(I)
               VALNEW = RATIO*VALOLD
               C0NEW(I)= VALNEW
500        CONTINUE
C
C
C...adjust variances (diagonal elements)
           N=NCOLD
           DO 501 J = 1,I6
           DO 502 I = J,NCOLD
               N=N+1
C...select only diagonal elements of soil or soil+routing
               IF (I .NE. J) GOTO 830
                   SDOLD = SQRT(COLD(N))
                   SDNEW = RATIO*SDOLD
C
C...find address of this element in new carryover array (which could
C...  be bigger than the old if NR has increased
                   NN = NCNEW
                    DO 5011 JJ=1,I6
                       DO 5021 II = JJ,NCNEW
                           NN=NN+1
C                          IF (II.EQ.I .AND. JJ.EQ.J) THEN
                           IF (II.NE.I .OR.  JJ.NE.J) GOTO 831
                               C0NEW(NN) = SDNEW*SDNEW
                               GO TO 502
831                        CONTINUE
5021                   CONTINUE
5011               CONTINUE
830            CONTINUE
502        CONTINUE
501        CONTINUE
C
C...adjust off-diagonal covariances
           N = NCOLD
C...process each row of either soil or soil+router
           DO 503 J=1,I6
C...process each column of old carryover array
           DO 504 I=J,NCOLD
               N=N+1
C...skip over diagonal elements and, if appropriate, router rows
               IF (I.EQ.J .OR.  I.GT.I6) GOTO 833
C...save old COV(I,J)
                  COVOLD = COLD(N)
C
C...find OLD STDEV(I,I)
                  NN = NCOLD
                  DO 505 JJ = 1,I6
                      DO 506 II = JJ,NCOLD
                          NN=NN+1
                          IF (II.NE.I .OR.  JJ.NE.I) GOTO 834
                              SDIIOL = SQRT(COLD(NN))
                              GO TO 5051
834                       CONTINUE
506                   CONTINUE
505               CONTINUE
C
C...locate new STDEV(I,I)
5051              NN = NCNEW
                  DO 5151 JJ=1,I6
                      DO 5061 II = JJ,NCNEW
                          NN = NN+1
                          IF (II.NE.I .OR.  JJ.NE.I) GOTO 835
                              SDIINE = SQRT(C0NEW(NN))
                              GO TO 5052
835                       CONTINUE
5061                  CONTINUE
5151              CONTINUE
C
C...old STDEV(J,J)
5052              NN = NCOLD
                  DO 507 JJ = 1,I6
                      DO 508 II = JJ,NCOLD
                          NN = NN+1
                          IF (II.NE.J .OR.  JJ.NE.J) GOTO 836
                              SDJJOL = SQRT(COLD(NN))
                              GO TO 5053
836                           CONTINUE
508                    CONTINUE
507                CONTINUE
C
5053               NN=NCNEW
                   DO 5071 JJ = 1,I6
                       DO 5081 II = JJ,NCNEW
                           NN = NN+1
                           IF (II.NE.J .OR.  JJ.NE.J) GOTO 837
                               SDJJNE = SQRT(C0NEW(NN))
                               GO TO 5054
837                        CONTINUE
5081                   CONTINUE
5071                CONTINUE
C
C...calculate COR(I,J)
5054               CORR   = COVOLD/SDIIOL/SDJJOL
C...compute new covariance COV(I,J)
                   COVNEW = CORR*SDIINE*SDJJNE
C...store new covariance in correct location in new carryover array
                   NN = NCNEW
                   DO 5082 JJ = 1,I6
                       DO 5083 II = JJ,NCNEW
                           NN = NN+1
                           IF (II.NE.I .OR.  JJ.NE.J) GOTO 838
                               C0NEW(NN) = COVNEW
                               GO TO 504
838                        CONTINUE
5083                   CONTINUE
5082               CONTINUE
833            CONTINUE
504        CONTINUE
503        CONTINUE
C
829    CONTINUE
C
C
C******* CHECK FOR CHANGE IN NUMBER OF ROUTING PARAMETERS *************
C IF NUMBER CHANGES, THEN COPY SOIL CARRYOVER FROM COLD TO C0NEW,
C  BUT LEAVE THE ELEMENTS OF C0NEW CORRESPONDING TO ROUTING
C  AS INITIAL CARRYOVER READ IN BY PIN ROUTINE
C
C      IF (IAREA.EQ.0 .AND. INR.EQ.1) THEN
       IF (IAREA.NE.0 .OR.  INR.NE.1) GOTO 840
C
C...soil state means are ALREADY copied to C0NEW after deficit adjustmen
C         DO 4004 N = 1,6
C4004       C0NEW(PY+N-1) = COLD(PY+N-1)
C
C...copy soil part of covariance matrix
           N=NCOLD
           DO 4001 J =1,6
               DO 401 I = J,NCOLD
                   N=N+1
                   IF (I .GT. 6) GOTO 841
                       NN=NCNEW
                       DO 4002 JJ = 1,6
                           DO 4003 II = JJ,NCNEW
                               NN=NN+1
                               IF (II.NE.I .OR.  JJ.NE.J) GOTO 842
                                   C0NEW(PY+NN-1) = COLD(PY+N-1)
                                   GO TO 401
842                            CONTINUE
4003                       CONTINUE
4002                   CONTINUE
841                CONTINUE
C
401            CONTINUE
4001       CONTINUE
C
840    CONTINUE
C
C
       VALOLD = POLD(PXM)
       VALNEW = P0NEW(PXM)
       IF (VALOLD .EQ. VALNEW) GOTO 844
           WRITE (IPR,35) VALOLD, VALNEW
35         FORMAT(/10X,'**WARNING** EXPONENT OF ',
     +      'ROUTING RESERVOIR CHANGED',/,15X,
     +      'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
844    CONTINUE
C
       VALOLD = POLD(PALPD)
       VALNEW = P0NEW(PALPD)
       IF (VALOLD .EQ. VALNEW) GOTO 845
           WRITE (IPR,351) VALOLD, VALNEW
351        FORMAT(/10X,'**WARNING** COEFFICIENT OF ROUTING',
     +      ' RESERVOIR CHANGED',/,15X,'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
845    CONTINUE
C
C
C******* CHECK CONSTANT DEVIATION & COEF OF VARIATION FOR DATA *********
C
       VALOLD = POLD(PP+INT(POLD(PPCSD))-1)
       VALNEW = P0NEW(PP+INT(P0NEW(PPCSD))-1)
       IF (VALOLD .EQ. VALNEW) GOTO 846
           WRITE (IPR,36) VALOLD,VALNEW
36         FORMAT(/10X,'**WARNING** M.A.P. CONSTANT STAND DEV',
     +      ' CHANGED',/,15X,
     +      'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
846    CONTINUE
C
       VALOLD = POLD(PP+INT(POLD(PPCV))-1)
       VALNEW = P0NEW(PP+INT(P0NEW(PPCV))-1)
       IF (VALOLD .EQ. VALNEW) GOTO 847
           WRITE (IPR,361) VALOLD,VALNEW
361        FORMAT(/10X,'**WARNING** M.A.P. COEF OF VARIATION',
     +      '  CHANGED',/,15X,'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
847    CONTINUE
C
       VALOLD = POLD(PP+INT(POLD(PECSD))-1)
       VALNEW = P0NEW(PP+INT(P0NEW(PECSD))-1)
       IF (VALOLD .EQ. VALNEW) GOTO 848
           WRITE (IPR,37) VALOLD, VALNEW
37         FORMAT(/10X,'**WARNING** EVAPOTRANSP CONST STAND DEV',
     +      ' CHANGED',/,15X,' FROM ',E10.3,' TO ',E10.3)
            CALL WARN
848    CONTINUE
C
       VALOLD = POLD(PP+INT(POLD(PECV))-1)
       VALNEW = P0NEW(PP+INT(P0NEW(PECV))-1)
       IF (VALOLD .EQ. VALNEW) GOTO 849
           WRITE (IPR,371) VALOLD,VALNEW
371        FORMAT(/10X,'**WARNING** EVAPOTRANSP COEF OF VARIATION',
     +      ' CHANGED',/,15X,'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
849    CONTINUE
C
       VALOLD = POLD(PR1)
       VALNEW = P0NEW(PR1)
       IF (VALOLD .EQ. VALNEW) GOTO 850
           WRITE (IPR,38) VALOLD, VALNEW
38         FORMAT(/10X,'**WARNING** DISCHARGE CONST STAND DEV',
     +      ' CHANGED',/,15X,'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
850    CONTINUE
C
       VALOLD = POLD(PR2)
       VALNEW = P0NEW(PR2)
       IF (VALOLD .EQ. VALNEW) GOTO 851
           WRITE (IPR,381) VALOLD, VALNEW
381        FORMAT(/10X,'**WARNING** DISCHARGE COEF OF VARIATION',
     +      ' CHANGED',/,15X,'FROM ',E10.3,' TO ',E10.3)
           CALL WARN
851    CONTINUE
C
       VALOLD = POLD(PALINP)
       VALNEW = P0NEW(PALINP)
       IF (VALOLD .EQ. VALNEW) GOTO 852
            WRITE (IPR,39) VALOLD, VALNEW
39          FORMAT(/10X,'**WARNING** COEF OF INPUT COMPONENT OF',
     +       ' MODEL-ERROR COVAR CHANGED',/,15X,'FROM ',
     +       E10.3,' TO ',E10.3)
             CALL WARN
852    CONTINUE
C
       VALOLD = POLD(PALPAR)
       VALNEW = P0NEW(PALPAR)
       IF (VALOLD .EQ. VALNEW) GOTO 853
           WRITE (IPR,40) VALOLD, VALNEW
40         FORMAT(/10X,'**WARNING** COEF OF PARAMETER COMPONENT',
     +      ' OF MODEL-ERROR COVAR CHANGED',/,15X,' FROM ',
     +      E10.3,' TO ',E10.3)
           CALL WARN
853    CONTINUE
C
       DO 302 I=1,16
           VALOLD = POLD(PPSTDV+I-1)
           VALNEW = P0NEW(PPSTDV+I-1)
           IF (VALOLD .EQ. VALNEW) GOTO 854
               WRITE (IPR,41) PNAME(I),VALOLD,VALNEW
41             FORMAT(/10X,'**WARNING** S.D. OF RAINFALL-RUNOFF',
     +          ' MODEL PARAMETER ESTIMATE CHANGED,',/,15X,'FOR ',
     +          A8,' : FROM ',E10.3,' TO ',E10.3)
               CALL WARN
854        CONTINUE
302    CONTINUE
C
       DO 410 I = 1,6
           VALOLD = POLD(PSCM+I-1)
           VALNEW = P0NEW(PSCM+I-1)
           IF (VALOLD .EQ. VALNEW) GOTO 855
               WRITE (IPR,43) CNAME(I),VALOLD,VALNEW
43             FORMAT(/10X,'**WARNING** SACRAMENTO MODEL',
     +          ' INITIAL SOIL CONDITION HAS CHANGED,',/,15X,
     +          'FOR: ',A8,' FROM ',E10.3,' TO ',E10.3)
               CALL WARN
855        CONTINUE
410    CONTINUE
C
       NMIN = MIN0(NROLD,NRNEW)
       NMAX = MAX0(NROLD,NRNEW)
C
       L2 = NMIN+6
       DO 405 I = 1,L2
           VALOLD = POLD(PSCMCV+I-1)
           VALNEW = P0NEW(PSCMCV+I-1)
           IF (VALOLD .EQ. VALNEW) GOTO 856
               WRITE (IPR,44) I,VALOLD,VALNEW
44             FORMAT(/10X,'**WARNING** DIAGONAL ELEMENTS',
     +          ' OF INITIAL STATE COVAR MATRIX CHANGED',/,15X,
     +          'FOR ELEMENT# ',I2,' FROM ',E10.3,' TO ',E10.3)
               CALL WARN
856        CONTINUE
405    CONTINUE
C
       IF (INR .NE. 1) GOTO 857
           L1 = 6+NMIN+1
           L2 = 6+NMAX
           DO 4051 I = L1,L2
               INDX = PSCMCV+I-1
               IF (NMAX .EQ. INT(P0NEW(PNR)) )
     +          WRITE (IPR,45) I,P0NEW(INDX)
               IF (NMAX .NE. INT(P0NEW(PNR)) )
     +          WRITE (IPR,46) I,POLD(INDX)
45             FORMAT(/10X,'**WARNING** NEW DIAGONAL ELEMENT',
     +         ' OF INITIAL STATE COVAR MATRIX ADDED',/,15X,
     +         'FOR ELEMENT# ',I2,' = ',E10.3)
46             FORMAT(/10X,'**WARNING** DIAGONAL ELEMENT OF',
     +         ' INITIAL STATE COVAR MATRIX DELETED',/,15X,
     +         'FOR ELEMENT# ',I2,' = ',E10.3)
4051       CONTINUE
857    CONTINUE
C
       IF (ITRACE .GT. 0) WRITE (IODBUG,991)
991    FORMAT(/10X,'** EXIT COX22.')
C
       RETURN
       END
