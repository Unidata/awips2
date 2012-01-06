C MEMBER PRED22
C **********************************************************************
C **********************************************************************
C
       SUBROUTINE PRED22(NTF,TEND1,IOFSET,DS,IBUG,IDB,DV)
C                             UPDATE: 01/19/94.12:20:28 BY $WC20SV
C							UPDATE: 01/05/01   BY KP GEORGAKAKOS	
C
C
C  STOCHASTIC PROPAGATION PART OF SS-SAC
C  CREATED BY KONSTANTINE P. GEORGAKAKOS, HRL/NWS, FALL 1984
C
C  MODIFIED FOR WMO, VANCOUVER INTERCOMPARISON PROJECT
C  BY KONSTANTINE P. GEORGAKAKOS, UI, NOV-1986
C
C  REVISION HISTORY:
C
C   HFS VERSION 1.3:
C 8-02-89  5:01:58 pm
C  changed to make compatible with OS FORTRAN IV (H-EXTENDED)
C
C   SS-SAC VERSION 1.6
C 1-5-2001  
C   changed to allow time variable coefficients of variation for MAPF
C
C****** NON-FC LABELED COMMON BLOCKS ***********************************
C
       COMMON/STAT22/ Y(91)
       COMMON/IUPD22/ INDX
       COMMON/STTU22/ YST(91)
       COMMON/SPDN22/ Q(12)
C***J.C.,890810
C      COMMON/PMOD22/ P(1000)
       COMMON/PMOD22/ P(70)
       COMMON/SUBD22/ SUBS
C
       COMMON/STAN22/ NC
       COMMON/ACCU22/ TOL
CKPG 01/05/01 ADDED ZFF(50) IN COMMON BLOCK OBSZ22
CJAS 20020827 changed max from 50 to 744 to accommodate 31 x 24 steps
       COMMON/OBSZ22/ ZPP(744),ZEE(744),ZHH,ZFF(744)
CKPG 12/2/97 ADDED ZSTDPD IN COMMON BLOCK OBSD22
       COMMON/OBSD22/ ZPD, ZED, ZHD, ZHPRED, ZSTDPD, ZHEST, KDA,
     +    KDAP, KHR, KHRP, NDM, ZDD
C***J.C.,890810
       COMMON/PFLAGS/ IFIRST, NCTIM
C
CKPG 01/05/01 ADDED ZFAC IN COMMON BLOCK OBSF22
       COMMON/OBSF22/ ZPRE,ZEVAP,ZDIS,ZFAC
       COMMON/TIMC22/ NHST
C
       COMMON/SQKM22/ AREA
       COMMON/TIME22/ CHR
C
C
       COMMON/DATS22/ NYRMO,NDAHR
       COMMON/TRAI22/ ITF
       COMMON/ARCH22/ XPR(2),XSPRH(2),PRBN(5),XHFT,XHFTH
       COMMON/SOIL22/ DROO
       COMMON/ADSP22/ ADY(12,12)
       COMMON/VARA22/ CFVMX
       COMMON/VARI22/ VARCHN
       COMMON/PARM22/ XM,ALP(6)
       COMMON/WRWM22/ IARCH,NR12,IST,NS,DIVIS
C
C******** FC LABELED COMMON BLOCKS NEEDED  *****************************
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
C...control information for saving carryover:
       COMMON /FCARY/IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
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
C
C***********************************************************************
C
C         COMMON/OBSZ/ ZPP(50),ZEE(50),ZHH
C
C
C************  OTHER VARIABLES  ****************************************
C
       EXTERNAL FLWS22
C
       DIMENSION DY(91),C(5),W(16,91),YAUX(91)
       INTEGER*4 MON(12)
CJAS..20020827 changed max from 50 to 744 to accommodate 31 x 24 steps
       DIMENSION HFT(744), HFSTD(744)
C
C...internal clock time for time step previous to one currently predicti
       INTEGER KDAP, KHRP
C
C...output time series array for results
       REAL DS(1)
       REAL DV(1)
C
C...offset to start of storage for this run in DS
       INTEGER IOFSET
C
       INTEGER NCTIM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sssac/RCS/pred22.f,v $
     . $',                                                             '
     .$Id: pred22.f,v 1.5 2002/10/10 16:02:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C************  DATA STATEMENTS  ****************************************
C
         DATA MON/4HJAN ,4HFEB ,4HMAR ,4HAPR ,4HMAY ,4HJUN ,4HJUL ,
     *   4HAUG ,4HSEPT,4HOCT ,4HNOV ,4HDEC /
C
C***********************************************************************
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,990)
990    FORMAT(/10X,'** PRED22 ENTERED.')
C
C
       N=NC+NC*(NC+1)/2
C
C  LOOP OF EXTENDED-FORECAST TIME-INCREMENTS
C
       DO 100 ITF=1,NTF
C
C
C...convert internal clock date/time to month,day,year,hour
           CALL MDYH1(KDA,KHR,IIMO,IIDA,IIYR,IIHR,
     +      NOUTZ,NOUTDS,TZC)
           IF (IST .GT. 1)
     +     CALL MDYH1(KDAP,KHRP,IIMOP,IIDAP,IIYRP,IIHRP,
     +      NOUTZ,NOUTDS,TZC)
C
           ZPRE=ZPP(ITF)
           ZEVAP=ZEE(ITF)
           ZDIS=ZHH
CKPG 01/05/01 STORE CURRENT FACTOR FOR MAPF CV
		 ZFAC=ZFF(ITF)
		 IF(NTF.GT.1.AND.ABS(ZFAC-1.0).GT.1.E-6) THEN
		    WRITE(IPR,*) '     MAP VARIANCE MODIFIER = ', ZFAC
                 ENDIF
C
           TOO=0.
           IF (IDB .LE. 0) GO TO 201
C
           DO 10 I=1,N
 10        YAUX(I)=Y(I)
C
           NR=NC-1
           DO 20 I=1,NR
               II1=I+1
               KK=(I-1)*NC+1
               IF(I.GT.2) KK=KK-(I-1)*(I-2)/2
               KKNC=KK+NC
               X1=Y(KKNC)
               DO 15 J=II1,NC
                   K=(J-1)*NC+1
                   IF(J.GT.2)K=K-(J-1)*(J-2)/2
                   KNC=K+NC
                   X2=Y(KNC)
                   X12=ABS(X1*X2)
                   X12=SQRT(X12)
                   K=KK+J-I
                   KNC=K+NC
                   YAUX(KNC)=Y(KNC)/X12
15             CONTINUE
20         CONTINUE
C
           WRITE(IODBUG,1) (YAUX(J),J=1,N)
C
201        CONTINUE
C
           CALL DVRK22 (N,FLWS22,TOO,Y,TEND1,TOL,IND,C,W)

C***K.G., 880701
           IF (IND .LE. 10) GOTO 801
               NSTEP = IST+ITF-1
               SUBNEW= SUBS+2
               WRITE (IPR,90) NSTEP,
     +          IIMO,IIDA,IIYR,IIHR,IND,SUBS,SUBNEW
90             FORMAT(/10X,'**WARNING** INTEGERATION RESULTS'
     +          ' MAY BE INACCURATE',/,
     +          15X,' FOR TIME STEP#',I4,4X,I2,'/',
     +          I2,'/',I4,4X,I2,':00',/,
     +          15X,' IND                            = ',I2,/,
     +          15X,' CURRENT VALUE OF SUBS PARAMETER= ',F5.1,/,
     +          15X,' RE-RUN WITH SUBS               = ',F5.1,/)
               CALL WARN
801        CONTINUE
C***
C
           IF (IDB .LE. 0) GO TO 111
C
           DO 21 I=1,N
 21        YAUX(I)=Y(I)
C
           NR=NC-1
           DO 23 I=1,NR
               II1=I+1
               KK=(I-1)*NC+1
C
               IF(I.GT.2) KK=KK-(I-1)*(I-2)/2
               KKNC=KK+NC
               X1=Y(KKNC)
               DO 22 J=II1,NC
                   K=(J-1)*NC+1
                   IF(J.GT.2)K=K-(J-1)*(J-2)/2
                   KNC=K+NC
                   X2=Y(KNC)
                   X12=ABS(X1*X2)
                   X12=SQRT(X12)
                   K=KK+J-I
                   KNC=K+NC
                   YAUX(KNC)=Y(KNC)/X12
22             CONTINUE
23         CONTINUE
C
           WRITE (IODBUG,2324)
2324       FORMAT(/10X,'DYNAMICS COEFFICEINT MATRIX:')
           DO 2321 IJ=1,NC
2321       WRITE (IODBUG,2322) IJ,(ADY(IJ,J),J=1,NC)
2322       FORMAT(15X,'ROW#: ',I2,/,
     +      (T23,6F8.2))
           WRITE(IODBUG,11) ITF,(YAUX(J),J=1,N)
11         FORMAT(/10X,'STATE AND UPPER TRIANGULAR COVARIANCE',
     +      ' PREDICTION',
     +      ' FOR ',I2,' FORECAST STEP IN MM UNITS : ',/,
     +      (15X,6F8.2))
C
           WRITE(IODBUG,921) DROO
921        FORMAT(/10X,'PREDICTED TOTAL CHANNEL INFLOW IN MM/DT:'
     +      5X,F8.2)
C
111        CONTINUE
C
C
C  FORECAST PRINTOUT
C
           NORGR=6
           QF=0.
           NR=NC-NORGR
           IF(Y(NC).GT.0.)QF=ALP(NR)*Y(NC)**XM
CKPG12/2/97 - START
C  ADD PREDICTED FLOW STANDARD DEVIATION
        STDF=1.E-4
        IF(Y(N).GT.0..AND.Y(NC).GT.0.)STDF=SQRT(Y(N))*ALP(NR)
     *     *XM*Y(NC)**(XM-1.)
	HFSTD(ITF) = STDF
CKPG12/2/97 - END
           HFT(ITF)=QF
C
C  SAVE FOR UPDATING
C
           IF (INDX.NE.ITF) GO TO 30
C
           DO 25 IJ=1,N
25         YST(IJ)=Y(IJ)
C
30         CONTINUE
C
C
           MONTH = MON(IIMO)
           IF (IDB .GE. 0) WRITE (IPR,8) MONTH,IIDA,IIYR,IIHR,ZDD,
     +      QF,SUBS
C
           IF (IDB .GT. 0) WRITE(IODBUG,3) IND
C
8          FORMAT(10X,A4,I2,', ',I4,2X,I2,':00',
     +      3X,'OBS= ',F8.2,2X,'PRED= ',F6.2,' MM/DT DISCH',
     +      3X,'SUBS= ',F2.0)
3          FORMAT(//20X,'NO. OF INTEGRATION DIVISIONS OF THIS STEP = ',
     +      I2)
 1         FORMAT(//,10X,'INITIAL STATE & UPPER TRIANGULAR COVARIANCE',
     +      ' IN MM UNITS  :  '/(15X,6F8.2))
C
C...print current line of data table to screen output
C           IF (IBUG.NE.1) GO TO 803 (Commented out to enforce output)
C
C...for previous time, print: pred discharge (calculated by prev step)
C                             obs  discharge (from this step)
C                             est  discharge (from this step)
               IF (ITF .EQ. 1) GOTO 805
                   ZPD = ZPP(ITF-1)
                   ZED = ZEE(ITF-1)
                   ZHD = -999.
805            CONTINUE
C
               CALL PRTD22 (IODBUG,IIMOP,IIDAP,IIYRP,
     +          IIHRP,ZPD,ZED,
CKPG 12/2/97 ADDED ZSTDPD IN THE SUBROUTINE ARGUMENT LIST
     +          ZHPRED,ZSTDPD,ZHD,ZHEST)
C...pred discharge for this time step
               ZHPRED = QF
CKPG12/2/97 - START
               ZSTDPD=STDF
CKPG12/2/97 - END
C803        CONTINUE (Commented out to enforce one-line output 4/2000)
C
C...if specified, save carryover from this point in the execution period
           IF (IST .EQ. 1) NCTIM = 1
           IF (IFILLC .NE. 1) GOTO 806
C              IF (NCSTOR.GT.0 .AND. NCTIM.LE.NCSTOR) THEN
               IF (NCSTOR.LE.0 .OR.  NCTIM.GT.NCSTOR) GOTO 807
C              IF (ICDAY(NCTIM).EQ.KDA .AND. ICHOUR(NCTIM).EQ.KHR) THEN
                   IF (ICDAY(NCTIM) .NE.KDA .OR.
     +                 ICHOUR(NCTIM).NE.KHR) GOTO 808
                       CALL FCWTCO (KDA,KHR,Y,NDM+1)
                       NCTIM = NCTIM+1
808                CONTINUE
807            CONTINUE
806        CONTINUE
C
C
C...variables indicating time at previous step
           KDAP = KDA
           KHRP = KHR
C
C...increment internal time zone clock
           KHR = KHR + NHST
           IF (KHR-1 .LE. 23) GOTO 809
               KHR = MOD(KHR-1,24)+1
               KDA = KDA+1
809        CONTINUE
C
C
100    CONTINUE
C
C
C
C...save the predicted discharge for all NF forecast steps
       IF (IST .LT. NS) GOTO 810
           DO 778 I=1,NTF
               DS(IOFSET+NS-1+I) = HFT(I)*AREA/3.6/DIVIS
	       DV(IOFSET+NS-1+I) = HFSTD(I)*AREA/3.6/DIVIS
778        CONTINUE
810    CONTINUE
C
       IF (ITRACE .GT. 1) WRITE (IODBUG,991)
991    FORMAT(/10X,'** EXIT PRED22.')
C
       RETURN
       END
