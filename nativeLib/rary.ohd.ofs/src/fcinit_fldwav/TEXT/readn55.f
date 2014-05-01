C MODULE READN55
C-----------------------------------------------------------------------
C
      SUBROUTINE READN55(PO,IPO,LEFTP,IUSEP,W,IUSEW,Z,IUSEZ,LEFTZ,
     1 CO,IUSEC,LEFTC,MSG,IERR)

C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     read system_name and scenario_name from segdef file
C     and store in P array
C

      CHARACTER*80 DESC
      CHARACTER *4 MSG(20,20)
      CHARACTER*16 RIVERJ
      COMMON/DIMS55/K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,K11,K12,K13,K14,K15,
     *            K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,
     *            K29,K30

      COMMON/FLP55/KFLP
      COMMON/GT55/KCG,NCG
      COMMON/KREV55/KREVRS
      COMMON/LEV55/NLEV,DHLV,NPOND,DTHLV,IDTHLV
      COMMON/METR55/METRIC
      COMMON/MIXX55/MIXFLO,DFR,FRC
      COMMON/M155/NU,JN,JJ,KIT,G,DT,TT,TIMF,F1
      COMMON/M655/KTIME,DTHYD,J1
      COMMON/M3055/EPSY,EPSQ,EPSQJ,THETA,XFACT
      COMMON/M3255/IOBS,KTERM,KPL,JNK,TEH
      COMMON/M3455/KXP,ICD,ITMAX,KWARM
      COMMON/NPC55/NP,NPST,NPEND
      COMMON/NYQDC55/NYQD
      COMMON/PRES55/KPRES
      COMMON/SS55/NCS,A,B,DB,R,DR,AT,BT,P,DP,ZH
      COMMON/TDBG55/TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2,LDBG1,LDBG2,MCMDBG
      COMMON/TPL55/DTHPLT
      COMMON/VS55/MUD,IWF,SHR,VIS,UW,PB,SIMUD
      COMMON/XNGZ55/NGZ,NGZN
      COMMON/TKEP55/DTHII,MDT,NDT,DTHS,TFH1
      COMMON/DTBRK55/DTFMN
cc      COMMON /OPFIL55/JFLOC,JFTIM,JFH,JFQ,JFHS,JFBS,JFBSL,JFBSR,JFBSS,
cc    .  JFFLD,JFUS,JFDS,JFTTL,ITYDS,JFXS,JFPK,JFGZ,JFOBS,JNCS,NUMTIM,
cc     .  JRIVR,NFGRF
      COMMON/MXVAL55/MXNB,MXNGAG,MXNCM1,MXNCML,MXNQL,MXINBD,MXRCH,
     .               MXMGAT,MXNXLV,MXROUT,MXNBT,MXNSTR,MXSLC
      COMMON/UNTS55/DSTNCE,XLNGTH,FLOW,SAREA,VOLUME,VELCTY,BSLOPE
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IDOS55/IDOS,IFCST
      COMMON/IONUM/IN,IPR,IPU
      COMMON/NETWK55/NET

      INCLUDE 'common/fldmap55'
      INCLUDE 'common/ofs55'
      INCLUDE 'common/opfil55'

      DIMENSION PO(*),IPO(*),CO(*),IFUT(10),W(1)
      DIMENSION UNITE(7),UNITM(7)
      CHARACTER*8  SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/readn55.f,v $
     . $',                                                             '
     .$Id: readn55.f,v 1.8 2004/09/24 22:46:41 jgofus Exp $
     . $' /
C    ===================================================================
C
      DATA UNITE/4hMILE,4hFEET,4hCFS ,4hACRE,4hFPS ,4hA FT,4hFPM /
      DATA UNITM/4hKM  ,4hM   ,4hCMS ,4hSQKM,4hM/S ,4hCU M,4h%   /
      DATA  SNAME / 8hREADN55  /

      CALL FPRBUG(SNAME,1,55,IBUG)
      IF (IBUG.EQ.1)
     .WRITE(IODBUG,2000)
 2000 FORMAT(
cc     #       /10X,'          PROGRAM FLDWAV - VERSION 1.0  9/30/98'
cc     #     ///10X,'                HYDROLOGIC RESEACH LABORATORY'
cc     #       /10X,'                  W/OH3 OFFICE OF HYDROLOGY'
cc     #       /10X,'                NOAA, NATIONAL WEATHER SERVICE'
cc     #       /10X,'                SILVER SPRING, MARYLAND  20910'
     .    ////10X,'                *******************************'
     .       /10X,'                *******************************'
     .       /10X,'                ***                         ***'
     .       /10X,'                ***  SUMMARY OF INPUT DATA  ***'
     .       /10X,'                ***                         ***'
     .       /10X,'                *******************************'
     .       /10X,'                *******************************'/)

      G=32.2

      LMSG=PO(10)-1
      NMSG=0
      DO 300 L=1,20
        READ(IN,2) (MSG(K,L),K=1,20)
CC      READ(IN,1) DESC
 1      format(A80)
 2      FORMAT(20A4)
cc      DESC=MSG(L)
CC      PO(LMSG+L)=DESC
        IF(MSG(1,L).EQ.'EOM ') GO TO 400
        NMSG=NMSG+1
        IF(IBUG.EQ.1) WRITE(IODBUG,2) (MSG(K,L),K=1,20)
  300 CONTINUE
      IF (MSG(1,L).NE.'EOM ') THEN
        IF (IBUG.EQ.1) WRITE(IODBUG,444)
  444   FORMAT('MAX OF 20 LINES ALLOWED... ALL OTHER LINES WILL BE ',
     .    'TRUNCATED')
  783   READ(IN,2,END=896) (MSG(K,L),K=1,20)
        IF(MSG(1,L).EQ.'EOM ') GO TO 400
        GO TO 783
  896   WRITE(IPR,247)
  247   FORMAT(/10X,'**ERROR**  NO EOM STATEMENT FOUND....PROGRAM TERMIN
     .ATED')
        GOTO 5000
      ENDIF

  400 PO(49)=NMSG+0.01
      NODESC=1
      READ(IN,1)DESC
      IF(DESC(1:1).EQ.'D') NODESC=0
      PO(26)=NODESC+0.01

C.......................................................................
C    READ/(PRINT) THE FOLLOWING GLOBAL VARIABLES AND STORE IN PO( )
C
C   11     EPSY     --  CONVERGENCE CRITERIA FOR STAGE
C   12     THETA    --  ACCELERATION FACTOR IN SOLVING TRIB JUNCTION PROB
C   13     F1       --  WEIGHTING FACTOR USED IN FINITE DIFFERENCE SCHEME
C   14     XFACT    --  FACTOR TO CONVERT LOCATION TO FEET
C   15     DTHYD    --  TIME INTERVAL (HRS) OF INPUT TIME SERIES
C   23     DTOUT    --  TIME INTERVAL (HRS) OF OUTPUT TIME SERIES
C   16     METRIC   --  METRIC OPTION SWITCH
C.......................................................................

      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,METRIC
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,20)
   20 FORMAT (/,'      EPSY     THETA        F1     XFACT     DTHYD',
     .         '     DTOUT    METRIC')
      IF(IBUG.EQ.1) WRITE(IODBUG,2091) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,
     . METRIC
 2091 FORMAT (6F10.3,4I10)
      ELSE
      IF(IBUG.EQ.1) WRITE(IODBUG,120) EPSY,THETA,F1,XFACT,DTHYD,DTOUT,
     . METRIC
  120 FORMAT(/
     .5X,'CONVERGENCE CRITERIA FOR STAGE                     EPSY',4X,
     .F10.3/5X,'ACCELERATION FACTOR IN SOLVING TRIB. JUNCTION PROB',1X,
     .'THETA',3X,F10.3/5X,'WEIGHTING FACTOR USED IN FINITE DIFFERENCE SC
     .HEME',2X,'F1',6X,F10.3/5X,'FACTOR TO CONVERT LOCATIONS TO FEET',
     .16X,'XFACT',3X,F10.3/5X,'TIME INTERVAL (HRS) OF INPUT HYDROGRAPHS'
     .,11X,'DTHYD',3X,F10.3/5X,'TIME INTERVAL (HRS) OF OUTPUT HYDROGRAPH
     .S',10X,'DTOUT',3X,F10.3/5X,'METRIC OPTION SWITCH',31X,'METRIC',2X,
     .I10)
      ENDIF

      PO(11)=EPSY
      PO(12)=THETA
      PO(13)=F1
      PO(14)=XFACT
      PO(15)=DTHYD
      PO(23)=DTOUT
      PO(16)=METRIC+0.01
C           CHECK TO SEE IF ALL TIME INTERVALS ARE ACCEPTABLE
C

C.......................................................................
C    17    JN       --  NUMBER OF RIVERS
C    18    NU       --  NO. OF DATA IN INPUT HYDROGRAPH
C    19    ITMAX    --  MAXIMUM NO. OF ITERATIONS ALLOWED IN NEWTON-
C                       RAPHESON TECHNIQUE.
C    20    KWARM    --  NO. OF WARM UP BEFORE ACTUALLY ROUTING
C    21    KFLD     --  SWITCH FOR FLOOD PLAIN USAGE
C    22    NET      --  NO. OF NETWORK RIVERS (CLOSE-ENDED)
C    25    ICOND    --  INITIAL CONDITIONS SWITCH
C          NMAP     --  NO. OF MAP SCENARIOS
C                       IF NMAP<0, INTERPOLATED SECTIONS USED FOR NSTR
C    26    NODESC   --  OUTPUT DISPLAY PARAMETER
C.......................................................................

      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,NMAP,
     .   (IFUT(I),I=1,2)
      IF(ITMAX.LE.0) ITMAX=10
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,30)
   30 FORMAT (/ '        JN        NU     ITMAX     KWARM      KFLP
     .  NET     ICOND      NMAP   FUTURE DATA')
      IF(IBUG.EQ.1) WRITE(IODBUG,32) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,
     . NMAP,(IFUT(I),I=1,2)
   32 FORMAT(8I10,3X,2I2)
      ELSE
      IF(IBUG.EQ.1) WRITE(IODBUG,130) JN,NU,ITMAX,KWARM,KFLP,NET,ICOND,
     .  NMAP,2
  130 FORMAT( /
     .5X,'NUMBER OF RIVERS IN THE SYSTEM',21X,
     .'JN',6X,I10/5X,'NO. OF POINTS IN INPUT HYDROGRAPHS',17X,
     .'NU',6X,I10/5X,'MAX NO. OF ITERATIONS IN NEWTON-RAPHSON TECHNIQUE'
     .,2X,'ITMAX',3X,I10/5X,'NO. OF TIMES TO WARM UP BEFORE ROUTING FLOW
     .',8X,'KWARM',3X,I10/5X,'FLOOD PLAIN USAGE OPTION SWITCH',20X,
     .'KFLD',4X,I10/5X,'NETWORK OPTION SWITCH',30X,'NET',5X,I10/5X,
     .'INITIAL CONDITIONS SWITCH',26X,'ICOND',3X,I10/5X,
     .'NO. OF MAP SCENARIOS',31X,'NMAP',4X,I10/5X,
     .'NUMBER OF FUTURE DATA POINTS',31X,I10)
      ENDIF

      IF(IDOS.GE.3.AND.NU.GT.0) NU=1
      MAPI=0
      IF(NMAP.LT.0) MAPI=1
      NMAP=IABS(NMAP)
      PO(17)=JN+0.01
      PO(18)=NU+0.01
      PO(19)=ITMAX+0.01
      PO(20)=KWARM+0.01
      PO(21)=KFLP+0.01
      PO(25)=ICOND+0.01
      PO(375)=NMAP+0.01
      PO(376)=IMAP+0.01
C.......................................................................
C    27    NYQD     -- NO.OF SETS OF STAGE-DISCHARGE VALUES
C    28    KCG      --  NUMBER OF DATA POINTS OF TIME SERIES FOR ALL
C                       INTERNAL BOUNDARIES
C    29    NCG      -- NUMBER OF GATES IN MOVABLE GATE OPTION
C    30    KPRES    -- COMPUTING HYDRAULIC RADIUS(R)SWITCH
C.......................................................................

      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) NYQD,KCG,NCG,KPRES
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,40)
   40 FORMAT (/ '      NYQD       KCG       NCG     KPRES')
      IF (IBUG.EQ.1) WRITE(IODBUG,2050) NYQD,KCG,NCG,KPRES
      ELSE
      IF (IBUG.EQ.1) WRITE(IODBUG,140) NYQD,KCG,NCG,KPRES
  140 FORMAT(/
     .5X,'NO.OF POINTS IN RATING CURVE AT D/S BOUNDARY',7X,
     .'NYQD',4X,I10/5X,'NO. OF POINTS IN SPILLWAY GATE CONTROL TIME ARRA
     .Y',2X,'KCG',5X,I10/5X,'MAX NO. OF GATES IN THE STRUCTURE',18X,
     .'NCG',5X,I10/5X,'COMPUTION OF HYDRAULIC RADIUS SWITCH',15X,'KPRES'
     .,3X,I10)
      ENDIF

      IF(IDOS.GE.3.AND.NYQD.GT.0) NYQD=1
      PO(27)=NYQD+0.01
      PO(28)=KCG+0.01
      PO(29)=NCG+0.01
      PO(30)=KPRES+0.01
C.......................................................................
C    31    NCS      -- NO. OF VALUES IN TABLE (BS) VS. (HS)
C    32    KPL      -- PARAMETER INDICATING INFO PLOTTED
C    33    JNK      -- HYDRAULIC PRINTING SWITCH
C    34    KREVRS   -- LOW FLOW FILTER SWITCH
C.......................................................................

      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) NCS,KPL,JNK,KREVRS,NFGRF
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,50)
   50 FORMAT(/ '       NCS       KPL       JNK    KREVRS     NFGRF')
      IF (IBUG.EQ.1) WRITE(IODBUG,2050) NCS,KPL,JNK,KREVRS,NFGRF
      ELSE
      IF (IBUG.EQ.1) WRITE(IODBUG,150) NCS,KPL,JNK,KREVRS,NFGRF
  150 FORMAT(/
     .5X,'NO. OF VALUES IN TABLE (BS) VS. (HS)               NCS',5X,I10
     ./5X,'PARAMETER INDICATING INFO PLOTTED                  KPL',5X,
     .I10/5X,'HYDRAULIC PRINTING SWITCH                          JNK',5X
     .,I10/5X,'LOW FLOW FILTER SWITCH                             KREVRS
     .',2X,I10/5X,'FLDGRF DATA GENERATION SWITCH',22X,
     .'NFGRF',3X,I10)
      ENDIF

CC      IF (IDOS.GE.3) KREVRS=1
      PO(31)=NCS+0.01
      KSTG=0
      PO(32)=KPL+0.01
      IF(KPL.LT.0) THEN
        KSTG=1
        PO(32)=KPL-0.01
        KPL=IABS(KPL)
      ENDIF
      PO(24)=KSTG+0.01
      IF(JNK.GT.0) THEN
        PO(33)=JNK+0.01
      ELSE
        PO(33)=JNK-0.01
      ENDIF
      PO(34)=KREVRS+0.01
      PO(35)=NFGRF+0.01

CC      IF (IDOS.GE.2) NFGRF=1
C.......................................................................
C    36    IOBS     -- OBSERVED DATA AT GAGING STATION SWITCH
c                      also denotes if com & obs data (iobs>1)
C    37    KTERM    -- EQUATION OF MOTION COMPUTED & PRINTED
C    38    NP       -- IF AUTOMATIC CALIBRATION OPTION IS USED
C    39    NPST     -- 1ST VALUE IN HYDROGRAPH USED IN STATS
C    40    NPEND    -- LAST VALUE IN HYDROGRAPH USED IN STATS
C.......................................................................

      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) IOBS,KTERM,NP,NPST,NPEND
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,80)
   80 FORMAT(/6X,4HIOBS,5X,5HKTERM,8X,2HNP,6X,4HNPST,5X,5HNPEND)
      IF (IBUG.EQ.1) WRITE(IODBUG,2050) IOBS,KTERM,NP,NPST,NPEND
      ELSE
      IF (IBUG.EQ.1) WRITE(IODBUG,180) IOBS,KTERM,NP,NPST,NPEND
  180 FORMAT( /
     .5X,'OBSERVED DATA AT GAGING STATION SWITCH             IOBS',4X,
     .I10/5X,'EQUATION OF MOTION COMPUTED & PRINTED              KTERM',
     .3X,I10/5X,'IF AUTOMATIC CALIBRATION OPTION IS USED            NP',
     .6X,I10/5X,'1ST VALUE IN HYDROGRAPH USED IN STATS              NPST
     .',4X,I10/5X,'LAST VALUE IN HYDROGRAPH USED IN STATS             NP
     .END',3X,I10/)
      ENDIF

      IF(NP.EQ.-3.OR.NP.EQ.-4) NCS=6

      IF(IOBS.GT.0) THEN
        PO(36)=IOBS+0.01
      ELSE
        PO(36)=IOBS-0.01
      ENDIF
      PO(37)=KTERM+0.01
      IF(NP.GT.0) THEN
        PO(38)=NP+0.01
      ELSE
        PO(38)=NP-0.01
      ENDIF
      PO(39)=NPST+0.01
      PO(40)=NPEND+0.01
C.......................................................................
C    41-48 FOR DEBUG PRINTING PARAMETERS
C        TDBG1    -- TIME ADDITIONAL DEBUG INFO BEGINS
C        TDBG2    -- TIME ADDITIONAL DEBUG INFO ENDS
C        JNKDBG   -- DEBUG SWITCH (JNK) FOR ADDITIONAL INFO
C.......................................................................

      TDBG1=99999.0
      TDBG2=0.0
      JDBG1=0
      JDBG2=0
      LDBG1=0
      LDBG2=0
      MCMDBG=0
      IF(JNK.GE.0) GO TO 57
      JNK=ABS(JNK)
      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2,LDBG1,LDBG2,
     * MCMDBG
C  53 FORMAT(2F10.2,I10)
      IF (IBUG.EQ.1) WRITE(IODBUG,54) TDBG1,TDBG2,JNKDBG,JDBG1,JDBG2
   54 FORMAT(/5X,'DETAIL DEBUG OUTPUT BETWEEN (TDBG1,TDBG2)= ',2F15.5,
     & /10X,'JNKDG=',I3,5X,'BETW RIVER (JDBG1,JDBG2)= ',2I5)
      IF (IBUG.EQ.1 .AND. NP.LT.0) WRITE(IODBUG,55) LDBG1,LDBG2,MCMDBG
   55 FORMAT(10X,'BETW MANNING N REACHES (LDBG1,LDBG2)= ',2I5,
     & 5X,'MCM=>',I3)

   57 PO(41)=TDBG1
      PO(42)=TDBG2
      PO(43)=JNKDBG+0.01
      PO(44)=JDBG1+0.01
      PO(45)=JDBG2+0.01
      PO(46)=LDBG1+0.01
      PO(47)=LDBG2+0.01
      PO(48)=MCMDBG+0.01
C.......................................................................
C    49    NMSG     --  NO. OF COMMENT LINES
C    50    TEH      --  TIME AT THE END OF THE RUN
C    51    DTHII    --  DT (hour) FOR IMPLICIT ROUTING
C    52    DTHPLT   --  DT FOR PLOT OF HYDROGRAPH
C    53    FRDFR    --  FROUDE WINDOW
C    54    DTEXP    --  DT FOR EXPLICIT ROUTING
C    55    MDT      --  Tr/MDT FACTOR
C    56    NDT      --  DIMENSION OF DTIN(TDTIN) ARRAY WHEN DTHII < 0
C.......................................................................
      NDT=0
      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT
      DTHS=DTHII
      IF(DTHYD.LE.0.) GO TO 86
      IF(DTHPLT.LT.DTHYD) GO TO 84
      IDT=DTHPLT/DTHYD+0.0001
      DTHPLT=DTHYD*IDT
      GO TO 86
   84 IF(DTHPLT.LE.0.) DTHPLT=DTHYD
      IDT=DTHYD/DTHPLT+0.0001
      DTHPLT=DTHYD/IDT
   86 CONTINUE
      IF(DTHPLT.LE.0.0 .AND. DTHII.GT.0.) DTHPLT=DTHII
      NUHH=NU
      NUH=NU
      IF(DTHPLT.GT.0.0 .AND. IDOS.LE.2) NUH=TEH/DTHPLT+1.01

      IFRC=FRDFR
      FRC=IFRC*0.1
      DFR=FRDFR-IFRC
      IF(DFR.LE.0.00) DFR=0.05
      IF(DFR.LT.0.01) DFR=0.01
      IF(DFR.GT.0.30) DFR=0.30
      IF(NODESC.EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,2175)TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT
      ELSE
      IF (IBUG.EQ.1) WRITE(IODBUG,275) TEH,DTHII,DTHPLT,FRDFR,DTEXP,MDT
  275 FORMAT(
     .5X,'TIME AT THE END OF THE RUN                         TEH',5X,
     .F10.3/5X,'DT (hour) FOR IMPLICIT ROUTING                     DTHII
     .',3X,F10.5/5X,'DT FOR PLOT OF HYDROGRAPH',26X,
     .'DTHPLT',2X,F10.5/5X,'FROUDE WINDOW',38X,
     .'FRDFR',3X,F10.2/5X,'DT FOR EXPLICIT ROUTING',28X,
     .'DTEXP',3X,F10.5/5X,'Tr/MDT FACTOR',38X,
     .'MDT',8X,I7/)
      ENDIF
      IF (MDT.EQ.0) MDT=20
      IF (DTHII.LT.0.0) NDT=INT(-DTHII)

      PO(50)=TEH
      PO(51)=DTHII
      PO(52)=DTHPLT
      PO(53)=FRDFR
      PO(54)=DTEXP
      PO(55)=MDT+0.01
      PO(56)=NDT+0.01
      IF(IDOS.NE.3) GO TO 87

      IF(DTHII.LT.0.000001) THEN
        WRITE(IPR,7003)
 7003   FORMAT(//10X,'**ERROR** A CONSTANT TIME STEP (DTHII > 0) IS ',
     1    'REQUIRED FOR THE NWSRFS APPLICATION OF FLDWAV.')
        CALL ERROR
        GO TO 87
      ENDIF

      IDHF=DTHYD+0.01
      KDHF=DTOUT+0.01
      ITVL=DTOUT/DTHII+0.0001
      TIM=ITVL*DTHII
C
      IF(DTOUT.LT.DTHII) THEN
        WRITE(IPR,7000) DTOUT,DTHII
 7000   FORMAT(//10X,'**ERROR** OUTPUT TIME SERIES INTERVAL(DTOUT=',
     1  F2.0,') MUST BE GREATER THAN OR EQUAL TO THE COMPUTATIONAL TIME'
     2  ,' STEP(DTHII=',F5.2,' ).')
        CALL ERROR
   10 ELSEIF(ABS(TIM-DTOUT).GE.0.0001) THEN
        WRITE(IPR,7001) DTOUT,DTHII
 7001   FORMAT(//,10X,'**ERROR** OUTPUT TIME SERIES INTERVAL (DTOUT=',
     1  F2.0,') IS NOT AN EVEN MULTIPLE OF THE COMPUTATIONAL TIME STEP',
     2  '(DTHII=',F5.2,' ).')
        CALL ERROR
      ENDIF
C
      IF(DTHYD.GE.DTHPLT) THEN
        KDHF=DTHYD/DTHPLT+0.01
        TDHF=KDHF*DTHPLT
        IF(ABS(TDHF-DTHYD).GE.0001) THEN
          WRITE(IPR,7002) DTHYD,DTHII,KITPR
 7002     FORMAT(//10X,'**WARNING** OBSERVED DATA TIME STEP ( DTHYD=',
     .    F2.0,') IS NOT AN EVEN INCREMENT OF THE TIME STEP FOR PLOT',
     2    'TING COMPUTED DATA'/22X,'(DTHII=',F5.2,')',2X,
     3    'THEREFORE THE OBSERVED DATA WILL NOT BE PLOTTED.')
          CALL WARN
        ENDIF
      ELSE
        KDHF=DTHPLT/DTHYD+0.01
        TDHF=KDHF*DTHYD
        IF(ABS(TDHF-DTHPLT).GE.0.0001) THEN
          WRITE(IPR,7002) DTHYD,DTHII,KITPR
          CALL WARN
        ENDIF
      ENDIF
C


 87   IF(NDT.LE.0) GO TO 90
      LODTIN=IUSEP+1
      LOTDTN=LODTIN+NDT
      IUSEP=LOTDTN+NDT-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(57)=LODTIN+0.01
      PO(58)=LOTDTN+0.01

      READ(IN,1,END=1000) DESC
      READ(IN,*) (PO(LODTIN+K-1),K=1,NDT)
      IF(IBUG.EQ.1) WRITE(IODBUG,92) (PO(LODTIN+K-1),K=1,NDT)
   92 FORMAT(/5X,'DTIN=',10F10.5/(10X,10F10.5))
      READ(IN,1,END=1000) DESC
      READ(IN,*) (PO(LOTDTN+K-1),K=1,NDT)
      IF(IBUG.EQ.1) WRITE(IODBUG,94) (PO(LOTDTN+K),K=1,NDT)
   94 FORMAT(4X,'TDTIN=',10F10.3/(10X,10F10.3))

      DTHS=PO(LODTIN)
      PO(LOTDTN+NDT-1)=2.0*TEH
          IF (DTHPLT.LE.0.0 .AND. NDT.GE.2) THEN
          DTHPLT=DTHS
          NUH=TEH/DTHS+1.01
          PO(52)=DTHPLT
          ENDIF

   90 IF(FRC.LE.0.0) FRC=1.0
 2050 FORMAT (8I10)
 2175 FORMAT(/'       TEH     DTHII    DTHPLT     FRDFR     DTEXP
     .MDT  '/ F10.3,2F10.5,F10.2,F10.5,2I10)

C.......................................................................
C    60    NLEV     --  TOTAL NO. OF LEVEE REACHES IN THE SYSTEM
C    61    DHLV     --  DIFF IN MAX & MIN CREST ELEVATIONS
C    62    DTHLV    --  STEP USED DURING LEVEE FAILURE
C.......................................................................
      READ(IN,1,END=1000) DESC
      READ(IN,*,END=1000) NLEV,DHLV,DTHLV
      IF(NODESC .EQ.1)THEN
      IF (IBUG.EQ.1) WRITE(IODBUG,2051) NLEV,DHLV,DTHLV
 2051 FORMAT(/'      NLEV      DHLV     DTHLV'/I10,2F10.5)
      ELSE
      IF (IBUG.EQ.1) WRITE(IODBUG,205)  NLEV,DHLV,DTHLV
  205 FORMAT(
     .5X,'TOTAL NO. OF CROSS-SECTION WITH LEVEES             NLEV',4X,
     .I10/5X,'DIFF IN MAX & MIN CREST ELEVATIONS                 DHLV',4
     .X,F10.5/5X,'STEP USED DURING LEVEE FAILURE                     DTH
     .LV',3X,F10.5)
      ENDIF
CC 2051 FORMAT(4X,5HNLEV=,I5,10X,'DHLV=',F8.5,10X,'DTHLV=',F10.5/)

      IDTHLV=0
      NPOND=0
      IF(DTHLV.LE.0.0) DTHLV=100000.0

      PO(60)=NLEV+0.01
      PO(61)=DHLV
      PO(62)=DTHLV
C.......................................................................
C        DETERMINE THE "K VALUES" BASED ON INPUT DATA
C
C        K1  =  NO. OF RIVERS IN THE SYSTEM
C        K3  =  NO. OF OBSERVED OR PLOTTING DATA POINTS
C        K6  =  NO. OF SETS OF POINTS IN THE D/S RATING CURVE TABLE
C        K8  =  NO. OF SETS OF POINTS IN THE B VS H TABLE
C        K9  =  NO. OF SETS OF POINTS IN THE BSS VS HSS TABLE
C        K22 =  NO. OF INTERPOLATED LEVEE REACHES IN THE SYSTEM
C        K25 =  NO. OF GAGES IN THE SYSTEM
C        K26 =
C        K27 =
C        K28 =
C        K29 =  MAX NO OF SLICES
C        K30 =  NO. OF MAPPING SCENARIOS
C.......................................................................

      K1=JN
      K3=NU
         IF(NUH.GT.K3) K3=NUH
         IF(NUHH.GT.K3) K3=NUHH
         IF(K3.EQ.0) K3=1
      IF(IDOS.LT.3) THEN
        K6=NYQD
      ELSE
        K6=112
      ENDIF
      IF(K6.EQ.0) K6=1
      K9=NCS
      K30=NMAP
      IF(K30.EQ.0) K30=1
      MPFRST=0

C        STORE THE UNITS FOR PRINTING
      IF(METRIC.EQ.0) THEN
        DSTNCE=UNITE(1)
        XLNGTH=UNITE(2)
        FLOW=UNITE(3)
        SAREA=UNITE(4)
        VELCTY=UNITE(5)
        VOLUME=UNITE(6)
        BSLOPE=UNITE(7)
      ELSE
        DSTNCE=UNITM(1)
        XLNGTH=UNITM(2)
        FLOW=UNITM(3)
        SAREA=UNITM(4)
        VELCTY=UNITM(5)
        VOLUME=UNITM(6)
        BSLOPE=UNITM(7)
      ENDIF
      PO(300)=  DSTNCE
      PO(301)=  XLNGTH
      PO(302)=  FLOW
      PO(303)=  SAREA
      PO(304)=  VELCTY
      PO(305)=  VOLUME
      PO(306)=  BSLOPE
      MXNGAG=0
      MXNCM1=0
      MXNCML=0
      MXNQL=0
      MXINBD=0
      MXMGAT=0
      MXRCH=0
      MXNSTR=0
C.......................................................................
C        STORE THE STARTING LOCATION OF THE FOLLOWING ARRAYS:
C  STARTING  ARRAY
C    LOC      NAME                DEFINITION
C
C  LONB      NBT                  NO. OF CROSS SECTIONS
C  LONQL     NQL                  NO. OF LATERAL INFLOWS
C  LONJUN    NJUN                 REACH NO. WHERE DYNAMIC TRIBUTARY
C                                 ENTERS MAIN RIVER.
C  LONPT     NPT
C  LOKU      KU                   UPSTREAM BOUNDARY SELECTION PARAMETER
C  LOKD      KD                   DOWNSTREAM BOUNDARY SELECTION PARAMETER
C  LONCM1    NRCM1                NO. OF MANNING N REACHES
C  LONGAG    NGAGE                NO. OF GAGING OR PLOTTING STATIONS
C  LOMIXF    MIXF
C  LONQCM    NQCM                 >0, DATA POINTS FOR MANNING N VS.
C                                 WATER ELEVATION
C                                 <0, DATA POINTS FOR MANNING N VS.
C                                 DISCHARGE CURVE
C                                 =0, MANNING N VS. WATER ELEVATION
C                                 CORRESPONDING TO
C  LOKFTR    KFTR(J)              KALMAN FILTER OPTION (1=ON 0=OFF)
C  LONSTR    NSTR(J)              NUM. OF OUTPUT T.S. (NWSRFS)
C  LOMRU     MRVU(J)              NET-RIVER CONNECTION-2
C  LONJUM    NJUM(J)              NET-RIVER JUNC-2

C  LOMPRV    MPRV(NMAP)           RIVER NO. OF MAPPING REACH
C  LOMPLC    MPLOC(2,NMAP)        U/S & D/S SECTION NO OF MAPPING REACH
C  LODTMP    DTMAP(NMAP)          TIME STEP FOR ANIMATION
C  LOSPTH    SYSPTH(NMAP)         RIVER SYSTEM NAME USED IN FLDVIEW PATH
C  LOTPTH    TWNPTH(NMAP)         TOWN NAME USED IN FLDVIEW PATH
C.......................................................................
      LONSTR=IUSEP+1
      LONBT=LONSTR+JN
      LONQL=LONBT+JN
      LONJUN=LONQL+JN
      LONPT=LONJUN+JN
      LOKU=LONPT+JN*2
      LOKD=LOKU+JN
      LONCM1=LOKD +JN
      LONGAG=LONCM1+JN
      LOMIXF=LONGAG+JN
      LONQCM=LOMIXF+JN
      LOMUD=LONQCM+JN
      LOUW=LOMUD+JN
      LOVIS=LOUW+JN
      LOSHR=LOVIS+JN
      LOPOWR=LOSHR+JN
      LOIWF=LOPOWR+JN
      LOKFTR=LOIWF+JN
cc      LOKFTR=LOMUD+JN
      LOKLPI=LOKFTR+JN
cc      LOXLOS=LOKLPI+JN
cc      LOQLOS=LOXLOS+JN*2
cc      LOALOS=LOQLOS+JN
CC      LOMRV=LOALOS+JN
      LOMRV=LOKLPI+JN
      LOKLOS=LOMRV+JN
      LCORDR=LOKLOS+JN
      LOMRU=LCORDR+JN
      LONJUM=LOMRU+JN
      IUSEP=LONJUM+JN

      IF(NLEV.GT.0) THEN
        LONJFT=IUSEP
        LONIFT=LONJFT+NLEV
        LONJTT=LONIFT+NLEV
        LONITT=LONJTT+NLEV
        LONXLV=LONITT+NLEV
        IUSEP=LONXLV+NLEV
      ENDIF

      IF(NMAP.GT.0) THEN
        LOMPRV=IUSEP
        LOMPLC=LOMPRV+NMAP
        LODTMP=LOMPLC+2*NMAP
        LOSPTH=LODTMP+NMAP
        LOTPTH=LOSPTH+NMAP*6
        IUSEP=LOTPTH+NMAP*6
      ENDIF
      IUSEP=IUSEP-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(63)=LONJFT+0.01
      PO(64)=LONIFT+0.01
      PO(65)=LONJTT+0.01
      PO(66)=LONITT+0.01
      PO(67)=LONXLV+0.0
      PO(68)=LONBT+0.01
      PO(69)=LONQL+0.01
      PO(70)=LONJUN+0.01
      PO(71)=LONPT+0.01
      PO(72)=LOKU+0.01
      PO(73)=LOKD+0.01
      PO(74)=LONCM1+0.01
      PO(75)=LONGAG+0.01
      PO(76)=LOMIXF+0.01
      PO(77)=LONQCM+0.01
      PO(78)=LOMUD+0.01
      PO(79)=LOUW+0.01
      PO(80)=LOVIS+0.01
      PO(81)=LOSHR+0.01
      PO(82)=LOPOWR+0.01
      PO(83)=LOIWF+0.01
      PO(84)=LOKFTR+0.01
      PO(85)=LOKLPI+0.01
cc      PO(86)=LOXLOS+0.01
cc      PO(87)=LOQLOS+0.01
cc      PO(88)=LOALOS+0.01
      PO(89)=LOMRV+0.01
      PO(90)=LOKLOS+0.01
      PO(91)=LCORDR+0.01
      PO(99)=LONSTR+0.01
      PO(369)=LOMRU+0.01
      PO(370)=LONJUM+0.01
      PO(371)=LOMPRV+0.01
      PO(372)=LOMPLC+0.01
      PO(377)=LODTMP+0.01
      PO(379)=LOSPTH+0.01
      PO(380)=LOTPTH+0.01

C------------   READ IN RIVER PROPERTIES --------------------
      CALL READ255(PO,PO,IUSEP,LEFTP,PO(LONBT),PO(LONQL),PO(LONJUN),
     +PO(LONPT),PO(LOKU),PO(LOKD),PO(LONCM1),PO(LONGAG),PO(LOMIXF),
     +PO(LONQCM),PO(LONJFT),PO(LONIFT),PO(LONJTT),PO(LONITT),NGZ,NGZN,
     +PO(LOKFTR),PO(LOKLPI),PO(LOMUD),PO(LOMRV),PO(LCORDR),PO(LOKLOS),
     +PO(LONSTR),PO(LOMRU),PO(LONJUM),PO(LOUW),PO(LOVIS),PO(LOSHR),
     +PO(LOPOWR),PO(LOIWF),KALMAN,PO(LOMPRV),PO(LOMPLC),PO(LODTMP),
     +PO(LOSPTH),PO(LOTPTH),NODESC,IERR,K1,K30)
      IF(IERR.EQ.1) GO TO 5000

      PO(22)=NET+0.01


      NRC=0
      IF(IPO(LOKD).EQ.3) NRC=1
      MXRCH=MXNBT
      IF(IDOS.NE.3) MXNSTR=0
      K4=MXNGAG
         IF(K4.EQ.0) K4=1
      K8=MXNCML
         IF(K8.EQ.0) K8=1
      K10=MXNQL
         IF(K10.EQ.0) K10=1
      K13=MXRCH
         IF(K13.EQ.0) K13=1
      K14=MXNSTR
         IF(K14.EQ.0) K14=1
      K18=NLEV
         IF(K18.EQ.0) K18=1
      K19=NCG
         IF(K19.EQ.0) K19=1
      K21=KCG
         IF(K21.EQ.0) K21=1
      K23=MXNBT
         IF(K23.EQ.0) K23=1

C  ..... THESE VALUES ARE SET TEMPORARILY - THERE ARE ARRAYS THAT NEED THEM
      K11=1
      K12=1
      K17=1
C.......................................................................

C  ..... THESE VALUES ARE NOT NEEDED AT ALL AND SHOULD BE DISCARDED
C          ALSO NOTE K13 USED TO BE # OF HGHs TO BE ACCESSED,
C                        NOW IS THE MAX # REACHES READ-IN
C                    K21 USED TO BE # OF AVGGAT
C                    K22 USED TO BE THE KALMAN FILTER SWITCH
C                        NOW IS THE MAX # SUBREACHES IS LEVEE REACH
C                    K14 USED TO BE # OF VALUES IN OCS B VS H TABLE
C                        NOW IS THE MAX # OUTPUT T.S. (NWSRFS ONLY)
C.......................................................................
C
      LONB=IUSEP+1
      LOXT=LONB+JN
      LODXM=LOXT+JN*MXNBT
      LORCHT=LODXM+JN*MXRCH
      LONLAD=LORCHT+JN*MXRCH
      LOLROT=LONLAD+JN
      IUSEP=LOLROT+JN-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(126)=LONB+0.01
      PO(127)=LOXT+0.01
      PO(128)=LODXM+0.01
      PO(129)=LORCHT+0.01
      PO(130)=LONLAD+0.01
      PO(131)=LOLROT+0.01
C---------------  READ IN XT/DXM/KRCH  --------------------------
      CALL RDRCH55(PO,PO(LONBT),PO(LORCHT),PO(LONLAD),
     * PO(LOXT),PO(LODXM),PO(LONB),PO(LONJFT),PO(LONIFT),PO(LONJTT),
     * PO(LONITT),JN,NODESC,IERR,K1,K13,K18,K23)
      IF(IERR.EQ.1) GO TO 5000

      MXRCH=MXNB-1
      MXNCM1=MXNB
      K2=MXNB
         IF(K2.EQ.0) K2=1
      K7=MXNCM1
         IF(K7.EQ.0) K7=1
      K15=K2*2

      NLOCK=PO(321)



C------------   KALMAN FILTER SETTING   -------------
      IF(KALMAN.EQ.0) GO TO 540
        KALMANP=0
        KALMANT=0
        DO 520 J=1,K1
        N=IPO(LONB+J-1)
        KFILT=IPO(LOKFTR+J-1)
        KALMANT=KALMANT+4*2*N*KFILT
        KALMANP=KALMANP+4*N**2*KFILT
  520   CONTINUE
        LCTFT=IUSEP+1
        LCY1FT=LCTFT+KALMANT
        LCY2FT=LCY1FT+2*K2
        LCPFT=LCY2FT+2*K2
        LCQFT=LCPFT+KALMANP
        LCHFT=LCQFT+4*K2*K2
        LCRFT=LCHFT+2*K2*K4
        LCZFT=LCRFT+K4*K4
        LCCFT=LCZFT+K4
        LCDFT=LCCFT+4*K2*K2
        LCEFT=LCDFT+4*K2*K2
        LCVFT=LCEFT+4*K2*K2
        LCAKFT=LCVFT+10*K2
        IUSEP=LCAKFT+2*K2*K4-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(335)=LCTFT+0.01
      PO(336)=LCY1FT+0.01
      PO(337)=LCY2FT+0.01
      PO(338)=LCPFT+0.01
      PO(339)=LCQFT+0.01
      PO(340)=LCHFT+0.01
      PO(341)=LCRFT+0.01
      PO(342)=LCZFT+0.01
      PO(343)=LCCFT+0.01
      PO(344)=LCDFT+0.01
      PO(345)=LCEFT+0.01
      PO(346)=LCVFT+0.01
      PO(347)=LCAKFT+0.01

C.......................................................................
C--------------   READ IN LEVEE(2)/DAM/BRIDGE DATA  ---------------
  540 CALL READ455(PO,IUSEP,LEFTP,PO(LONBT),PO(LONJFT),PO(LONJTT),
     * PO(LONIFT),PO(LONITT),PO(LORCHT),PO(LOLROT),PO(LOXT),PO(LOMIXF),
     * JN,NP,DTFMN,NU,NODESC,K1,K3,K13,K16,K18,K19,K20,K21,K23,W,IUSEW,
     * NRC,IERR)
      IF(IERR.EQ.1) GO TO 5000

      PO(378)=MXINBD+0.01
      NYQD=NRC
      PO(27)=NYQD+0.01
      IF(DTFMN.LT.900) NUHH=TEH/DTFMN+1.01
      IF(DTHII.GT.0.) NUHP=TEH/DTHII+1.01
      IF(DTFMN.GT.900.AND.DTHII.GT.0.) NUHH=NUHP
      IF(IDOS.LE.2 .AND. NUHH.GT.K3) K3=NUHH
      IF(NUHP.LT.NUHH) NUHP=NUHH
      IF(NUHP.LT.K3) NUHP=K3
      K24=NUHP*2
          IF(NFGRF.EQ.1) K24=1
      K22=MXNXLV
          IF(K22.EQ.0) K22=1

      DTI=DTHII
      IF(DTFMN.GT.0.0.AND.DTFMN.LT.DTI) DTI=DTFMN
      IF(DTI.LE.0.) DTI=DTHPLT
      PO(133)=DTI


      NTQL=PO(313)
      IF(MXNQL.GT.0) THEN
        LOLQ1=IUSEP+1
        IUSEP=LOLQ1+K10*K1-1
        IF(IDOS.GE.3) THEN
          LOQLNM=IUSEP+1
          IUSEP=LOQLNM+3*NTQL-1
        ENDIF
      ENDIF

      NTGAG=PO(323)
      K25=NTGAG
         IF(K25.EQ.0) K25=1
      IF(MXNGAG.GT.0) THEN
        LONGS=IUSEP+1
        LOSTTN=LONGS+K4*K1
        IUSEP=LOSTTN+5*NTGAG-1
        IF(IOBS.GE.1.AND.KPL.NE.2) THEN
          LOGZ=IUSEP+1
          IUSEP=LOGZ+K4*K1-1
        ENDIF
        IF(IOBS.GE.1.AND.KPL.EQ.3.AND.IDOS.GE.3) THEN
          LOSTQN=IUSEP+1
          IUSEP=LOSTQN+5*NTGAG-1
        ENDIF
CC        LOSTTN=IUSEP+1
CC        IF(IDOS.LT.3) THEN
CC          IUSEP=LOSTNM+5*NTGAG-1
CC        ELSE
CC          IUSEP=LOSTNM+3*NTGAG-1
CC        ENDIF
      ENDIF

      NTOUT=PO(331)
      IF(MXNSTR.GT.0) THEN
        LONST=IUSEP+1
        LCKTYP=LONST+K14*K1
        LOGZO=LCKTYP+K14*K1
        IUSEP=LOGZO+K14*K1-1
        IF(IDOS.GE.3) THEN
          LOSTON=IUSEP+1
          IUSEP=LOSTON+3*NTOUT-1
        ENDIF
      ENDIF

      IF(NU.NE.0) THEN
        LOGZ1=IUSEP+1
        IUSEP=LOGZ1+K1-1
        IF(IDOS.GE.3) THEN
          LOST1N=IUSEP+1
          IUSEP=LOST1N+3*JN-1
        ENDIF
      ENDIF

      LOSTM=IUSEP+1
      LOSLFI=LOSTM+K1
      IUSEP=LOSLFI+K2*K1-1

      IF(NLOCK.GT.0.AND.IDOS.GE.3) THEN
        LOPLNM=IUSEP+1
        LOIGNM=LOPLNM+NLOCK*3
        IUSEP=LOIGNM+NLOCK*3-1
      ENDIF

      IF(NGZN.GT.0) THEN
        LOGZN=IUSEP+1
        IUSEP=LOGZN+JN-1
      ENDIF

      IF(NRC.GT.0) THEN
        LCRC=IUSEP+1
        IUSEP=LCRC+NRC*2-1
      ENDIF

      IF(IPO(LOKD).EQ.0) THEN
        LONOSN=IUSEP+1
        LORIVN=LONOSN+3
        LOTIDN=LORIVN+3
        IUSEP=LOTIDN+3-1
      ENDIF

      IF(IOBS.GT.1) THEN
        LOSTEN=IUSEP+1
        IUSEP=LOSTEN+3*NTGAG-1
      ENDIF

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

        PO(132)= LOLQ1+0.01
        PO(211)= LONGS+0.01
        PO(212)= LOGZ+0.01
        PO(221)= LOGZ1+0.01
        PO(222)= LOGZN+0.01
        PO(229)= LOSLFI+0.01
cc        IF(IPO(LOKD).EQ.0.AND.IDOS.GE.3) THEN
          PO(348)= LONOSN+0.01
          PO(351)= LOTIDN+0.01
cc        ENDIF
        PO(349)= LORIVN+0.01
        PO(350)= LOQLNM+0.01
        PO(352)= LOSTTN+0.01
        PO(353)= LOSTEN+0.01
        PO(354)= LOSTQN+0.01
        PO(356)= LOSTON+0.01
        PO(358)= LONST+0.01
        PO(359)= LCKTYP+0.01
        PO(360)= LOGZO+0.01

          PO(361)= LOST1N+0.01
          PO(363)= LOSTM+0.01
          PO(366)= LCRC+0.01
CC        PO(362)= LOSTNN+0.01
cc        IF(NLOCK.GT.0.AND.IDOS.GE.3) THEN
          PO(367)= LOPLNM+0.01
          PO(368)= LOIGNM+0.01
cc        ENDIF

CC      ENDIF
C---------------  READ IN QL/GAGE/US/DS/ T.S. DATA  ---------------
      IF(IDOS.EQ.3) THEN
        CALL RHNWS55(PO,IUSEP,LEFTP,PO(LOKU),PO(LOKD),PO(LONGAG),
     *   PO(LONGS),PO(LOGZ),PO(LOSTTN),PO(LOSTQN),PO(LONQL),PO(LOLQ1),
     *   PO(LOQLNM),PO(LONB),PO(LONSTR),PO(LONST),PO(LCKTYP),PO(LOGZO),
     *   PO(LOSTON),PO(LOSTM),PO(LOGZ1),PO(LOGZN),PO(LOST1N),PO(LOSTNN),
     *   PO(LONOSN),PO(LORIVN),PO(LOTIDN),PO(LOSTEN),
     *   PO(LOSLFI), PO(LOPLNM),PO(LOIGNM),PO(LONLAD),PO(LOLAD),
     *   PO(LORCHT),PO(LONBT),XFACT,NRC,NODESC,IERR,K1,K2,K3,K4,K10,K13,
     *   K14,K16,K23,K25)
        IF(IERR.EQ.1) GO TO 5000
      ELSE
        CALL REDHYD55(PO,IUSEP,LEFTP,Z,IUSEZ,LEFTZ,PO(LOKU),PO(LOKD),
     *   PO(LONGAG),PO(LONGS),PO(LOGZ),PO(LOSTTN),PO(LONQL),PO(LOLQ1),
     *   PO(LONB),PO(LOGZ1),PO(LOSTM),PO(LOGZN),PO(LOSLFI),PO(LONLAD),
     *   XFACT,NODESC,IERR,K1,K2,K3,K4,K10,K23,K25)
        IF(IERR.EQ.1) GO TO 5000
      ENDIF

      IF(DTHII.EQ.0.0 .AND.DTHPLT.EQ.0.0) THEN
        DTHPLT=DTHS
        NUPLT=TEH/DTHPLT+1.01
        IF(K3.LT.NUPLT) K3=NUPLT
        IF(NFGRF.NE.1.AND.K3.GT.K24) K24=2*K3
        PO(52)=DTHPLT
      ENDIF

      LOIFXC=IUSEP+1
      IUSEP=LOIFXC+MXNB*JN-1

      IF(NFGRF.NE.1) THEN
        LORIVR=IUSEP+1
        IUSEP=LORIVR+20*(JN+1)-1
      ENDIF

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(231)=LOIFXC+0.01
      PO(365)=LORIVR+0.01

C-------------  READ IN CROSS-SECTION/MANNING'S N DATA -----------
      CALL READ355(PO,IUSEP,LEFTP,PO(LONBT),PO(LONGAG),PO(LONQCM),
     * PO(LONCM1),PO(LOIFXC),PO(LONGS),PO(LOXT),PO(LOSLFI),XFACT,JN,NCS,
     * NP,NODESC,IERR,K1,K2,K4,K9,K23)
      IF(IERR.GT.0) GO TO 5000

      IF(IOBS.GT.1) CALL RDSTAT55(PO,PO,IUSEP,LEFTP,JN,IOBS,PO(LONBT),
     . PO(LONGAG),PO(LONGS),PO(LONCM1),PO(LONQCM),PO(LONCM),PO(LOYQCM),
     . PO(LOKD),NODESC,IERR,K1,K4,K7,K8)

      PO(324)=LONSLC+0.01
      PO(325)=LONQSL+0.01
      PO(326)=LOSLIC+0.01
      PO(327)=LOFRMO+0.01
      PO(328)=LOFBIO+0.01
      PO(329)=LORRMO+0.01
      PO(330)=LORBIO+0.01

      IF(IERR.GT.0) GO TO 5000


      IUSEC=1
      LXYDI=IUSEC
      LXQDI=LXYDI+K1*K2
      IUSEC=LXQDI+K1*K2-1

      IF(NTQL.GT.0) THEN
        LXQLI=IUSEC+1
        IUSEC=LXQLI+NTQL-1
      ENDIF

      IF(NLOCK.GT.0) THEN
        LXPLTI=IUSEC+1
        LXIWTI=LXPLTI+NLOCK
        IUSEC=LXIWTI+NLOCK-1
      ENDIF

      CALL CHECKC(IUSEC,LEFTC,NERR)
      IF(NERR.EQ.1) THEN
        IUSEC=0
        GO TO 5000
      ENDIF

      PO(316)=LXYDI+0.01
      PO(317)=LXQDI+0.01
      PO(318)=LXQLI+0.01
      PO(319)=LXPLTI+0.01
      PO(364)=LXIWTI+0.01
      PO(3)=IUSEC

      IERR=0
      IF(ICOND.GT.0) CALL REDIC55(CO,CO,PO(LONB),PO(LONQL),
     .  PO(LONLAD),CO(LXYDI),CO(LXQDI),CO(LXQLI),CO(LXPLTI),CO(LXIWTI),
     .  JN,NTQL,PO(LORCHT),PO(LOLAD),NLOCK,IERR,IDOS,K16,K13,
     .  K2,K1)
      IF(IERR.GT.0) GO TO 5000

  610 IF(NFGRF.NE.1) CALL REDRVR55(PO(LORIVR),PO(LORIVR+20),JN)


  630 K5=MXROUT
         IF(NP.LT.0.AND.K4.GT.K5) K5=K4
         IF(K5.EQ.0) K5=1

      IPRT=0
      LCKRTP=IUSEP+1
      LCKRT1=LCKRTP+K1*K5
      LCKRTN=LCKRT1+K1*K5
      LOKRCH=LCKRTN+K1*K5
      IUSEP=LOKRCH+K1*K2-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(270)=LCKRTP+0.01
      PO(271)=LCKRT1+0.01
      PO(272)=LCKRTN+0.01
      PO(273)=LOKRCH+0.01

      CALL ROUTYP55(PO(LONBT),NP,PO(LONGAG),PO(LONGS),PO(LORCHT),
     1 PO(LOLROT),PO(LCKRTP),PO(LCKRT1),PO(LCKRTN),PO(LOMIXF),KEXP,
     2 K1,K4,K5,K13)

      IF(KEXP.EQ.0) GO TO 650

      LCEXPP=IUSEP+1
      LCEXPR=LCEXPP+K9*K2*K1
      IUSEP=LCEXPR+K9*K2*K1-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        IERR=1
        GO TO 5000
      ENDIF
      PO(149)=LCEXPP+0.01
      PO(150)=LCEXPR+0.01



  650 IF(NLEV.GT.0) THEN
        LONJFM=IUSEP+1
        LONIFM=LONJFM+MXNXLV
        LONJTO=LONIFM+MXNXLV
        LONITO=LONJTO+MXNXLV
        IUSEP=LONITO+MXNXLV-1
      ENDIF
      LOASS=IUSEP+1
      IUSEP=LOASS+K1*K2*K9-1
      PO(274)=LONJFM+0.01
      PO(275)=LONIFM+0.01
      PO(276)=LONJTO+0.01
      PO(277)=LONITO+0.01
      PO(278)=LOASS+0.01


      PO(101)=K1 +0.01
      PO(102)=K2 +0.01
      PO(103)=K3 +0.01
      PO(104)=K4 +0.01
      PO(105)=K5 +0.01
      PO(106)=K6 +0.01
      PO(107)=K7 +0.01
      PO(108)=K8 +0.01
      PO(109)=K9 +0.01
      PO(110)=K10+0.01
      PO(111)=K11+0.01
      PO(112)=K12+0.01
      PO(113)=K13+0.01
      PO(114)=K14+0.01
      PO(115)=K15+0.01
      PO(116)=K16+0.01
      PO(117)=K17+0.01
      PO(118)=K18+0.01
      PO(119)=K19+0.01
      PO(120)=K20+0.01
      PO(121)=K21+0.01
      PO(122)=K22+0.01
      PO(123)=K23+0.01
      PO(124)=K24+0.01
      PO(125)=K25+0.01

      IF(IBUG.EQ.1) WRITE(IODBUG,524) K1,K2,K3,K4,K5,K6,K7,K8,K9,K10,
     . K13,K14,K15,K16,K18,K19,K20,K21,K22,K23,K24,K25
  524 FORMAT(///30X,'SUMMARY OF ARRAY SIZES'//10X,
     . 'NO. OF RIVERS IN THE SYSTEM ...........................',I5/10X,
     . 'MAXIMUM NO. OF CROSS SECTIONS ON ANY RIVER ............',I5/10X,
     . 'NO. OF COMPUTATIONAL TIME STEPS .......................',I5/10X,
     . 'MAXIMUM NO. OF GAGING STATIONS ON ANY RIVER ...........',I5/10X,
     . 'MAXIMUM NO. OF ROUTING TECHNIQUES IN THE SYSTEM .......',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE D/S RATING CURVE TABLE ...',I5/10X,
     . 'MAXIMUM NO. OF MANNING N REACHES ON ANY RIVER .........',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE MANNING N TABLE ..........',I5/10X,
     . 'NO. OF SETS OF POINTS IN THE BS VS HSS TABLE ..........',I5/10X,
     . 'MAXIMUM NO. OF LATERAL FLOW HYDROGRAPHS ON ANY RIVER ..',I5/10X,
     . 'MAXIMUM NO. OF REACHES ON ANY RIVER ...................',I5/10X,
     . 'MAXIMUM NO. OF OUTPUT TIME SERIES (NWSRFS ONLY) .......',I5/10X,
     . 'MAXIMUM NO. OF EQUATIONS TO BE SOLVED (K2*2) ..........',I5/10X,
     . 'MAXIMUM NO. OF INTERNAL BOUNDARIES ON ANY RIVER .......',I5/10X,
     . 'TOTAL NO. OF LEVEE REACHES IN THE SYSTEM ..............',I5/10X,
     . 'MAXIMUM NO. OF MULTIPLE GATES ON ANY RIVER ............',I5/10X,
     . 'NO. OF DAMS WHICH HAVE MULTIPLE GATES .................',I5/10X,
     . 'NO. OF POINTS IN THE MOVABLE GATE TIME SERIES .........',I5/10X,
     . 'NO. OF INTERPOLATED LEVEE REACHES IN THE SYSTEM .......',I5/10X,
     . 'MAXIMUM NO. OF ACTUAL CROSS SECTIONS ON ANY RIVER .....',I5/10X,
     . 'TOTAL NO. OF HYDROGRAPH POINTS USED IN FLDGRF PROGRAM .',I5/10X,
     . 'TOTAL NO. OF GAGING STATIONS IN THE SYSTEM ............',I5)

      IF (IBUG.EQ.1) WRITE(IODBUG,525)
 525  FORMAT(//////30X,32(1H*)/30X,32(1H*)/30X,3(1H*),26X,3(1H*)/30X,
     1   32H***  SUMMARY OF OUTPUT DATA  ***/30X,3(1H*),26X,3(1H*)/
     2   30X,32(1H*)/30X,32(1H*)///)
      LCNN   =IUSEP+1
      LCLQN  =LCNN+K1*K23
      LCLQT  =LCLQN+K1*K10
      LCQUSJ =LCLQT+K1*K10
      LCIRGM =LCQUSJ+K3*K1
      LCIFLV =LCIRGM+K13*K1
      LCHLV  =LCIFLV+K22
      LCBLV  =LCHLV+K22
      LCYDIT =LCBLV+K22
      LCQDIT =LCYDIT+K1*K23
      LCDDX  =LCQDIT+K1*K23
      LCWF   =LCDDX+K1*K2
      LCIFCV =LCWF+K1
      LCHCAV =LCIFCV+K9*K2*K1
      LCDXR  =LCHCAV+K9*K2*K1
      LCXYZ  =LCDXR+K13*K1
      MXTP=K2
      IF(MXTP.LT.K14) MXTP=K14
      IUSEP  =LCXYZ+MXTP-1

       IF (KPRES.EQ.1) THEN
         LCPR=IUSEP+1
         IUSEP=LCPR+K1*K2*K9-1
       ENDIF
       IF (KFLP.GE.1) THEN
         LCBEV =IUSEP+1
         LCNKC =LCBEV+K1*K2*30
         IUSEP  =LCNKC+K1*K2-1
       ENDIF

      IF(NRC.GT.0) THEN
        LCYQD=IUSEP+1
        LCQYQD=LCYQD+112
        LCRCP=LCQYQD+112
        IUSEP=LCRCP+K16*K1-1
      ENDIF

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      PO(223)= LCQYQD
      PO(224)= LCYQD
      PO(225)= LCRCP
      PO(281)= LCNN+0.01
      PO(282)= LCLQN+0.01
      PO(283)= LCLQT+0.01
      PO(284)= LCQUSJ+0.01
      PO(285)= LCPR+0.01
      PO(286)= LCIRGM+0.01
      PO(287)= LCIFLV+0.01
      PO(289)= LCHLV+0.01
      PO(290)= LCBLV+0.01
      PO(291)= LCBEV+0.01
      PO(292)= LCNKC+0.01
      PO(293)= LCYDIT+0.01
      PO(294)= LCQDIT+0.01
      PO(295)= LCDDX+0.01
      PO(296)= LCWF+0.01
      PO(297)= LCIFCV+0.01
      PO(298)= LCHCAV+0.01
      PO(299)= LCDXR+0.01
      PO(307)= LCXYZ+0.01
      PO(308)= DTHS
      PO(309)= TFH1
C
C.......................................................................
C        DETERMINE THE AMOUNT OF WORK SPACE NEEDED IN THE D ARRAY
C        FOR THE FOLLOWING PARAMETERS (74):
C
C
C           LCC,LCD,LCFHT,LCICT,LCIPSV,LCIRSV,LCITRX,LCMINT,LCNDPS,
C           LCNSTR,LCQC,LCQD,LCQI,LCQII,LCQJ,LCQU,LCSTYP,LCVC,LCVD,
C           LCVU,LCXX,LCYC,LCYD,LCYII,LCYJ,LCYU,LCQINT,LCYINT,LCFFS,
C           LCFS,LCITSV,LCQA,LCQLJ,LCQTC,LCSTC,LCTII,LCVLSV,LCYA,LCQLLT,
C           LCQLSM,LCDHEQ,LCNQRT,LCQLRT,LCBBP,LCYBP,LCQBCH,LCQOTP,LCQOTR,
C           LCTFDB,LCIORF,LCTFDO,LCQDSN,LCHDSN,LCSQW,LCSQS1,LCSQS2,LCSQO,
C           LCLRMX,LCICTR,LCYUMN,LCIFR,LCKSP,LCKS1,LCKSN,LCYN,LCYCR,
C           LCFLAG,LCWDSN,LCTYPK,LCYPK,LCTQPK,LCQPK,LCQLV,LCQPND,LCPLTM,
C           LCQMX,LCYMX,LCRMS,LCAVD,LCNFLO,LCVPK,LCBKT,LCQKT,LCERQX,
C           LCSNMT,LCTFT,LCY1FT,LCY2FT,LCPFT,LCQFT,LCHFT,LCRFT,
C           LCZFT,LCCFT,LCDFT,LCEFT,LCVFT,LCAKFT,LCEXPP,LCEXPR
C
C
      LZEND=K1*8 + K2*2 + K11 + K12*4 +K13 + K15*5 + K22 + K24 +
     .      K1*K2*21 + K1*K4*2 + K1*K5 + K1*K16*22 + K2*K18*2 +
     .      K1*K2*K5*4 + K1*K2*K11*2 + 500 + 501*3 + 225 + IUSEC
C
      PO(4)=LZEND+0.01


      GO TO 9000

 1000 WRITE(IPR,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     *GLOBAL PARAMETERS.'/)
 5000 IERR=1
 9000 RETURN
      END





