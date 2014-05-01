C MODULE EX59
C-----------------------------------------------------------------------
C
C     THIS IS THE EXECUTION ROUTINE FOR DWOPER ASTORIA BALANCE REVIEW
C
C     THIS ROUTINE INITIALLY WRITTEN BY
C        JOANNE R. SALERNO - NWRFC   OCT 1997
C
C     POSITION     CONTENTS OF P ARRAY
C        1         VERSION NUMBER OF OPERATION
C        2-19      GENERAL NAME OR TITLE
C       20-22      DW OBSERVED ASTORIA STAGE (TIDE)
C       23-25      NOS         ASTORIA STAGE (STID)
C       26-28      OBS/FX  MAX/MIN BALANCE ASTID1  (SSTG)
C       29-31      OBS/FX  MAX/MIN BALANCE ASTID2  (SSTG)
C       32-34      OBS/FX  MAX/MIN BALANCE ASTID3  (SSTG)
C       35-37      OBS/FX  MAX/MIN BALANCE ASTID4  (SSTG)
C
C     THE NUMBER OF ELEMENTS REQUIRED IN THE P ARRAY IS  37 
C
C     POSITION     CONTENTS OF C ARRAY
C
C     THE NUMBER OF ELEMENTS REQUIRED IN THE C ARRAY IS   0
C
C       FLOW CHART FOR DWOPER ASTORIA TIDE REVIEW
C
C       1.      READ IN TIME SERIES
C
C                       DW OBS ASTORIA STAGE    (TID ) -- ODEPTH
C                       NOS    ASTORIA STAGE    (STID) -- PDEPTH
C
C       2.      IDENTIFY FIRST AND LAST HOURLY TIME SERIES
C               IDENTIFY LAST OBSERVED  HOURLY TIME SERIES
C                ITS            LOTS              LTS
C                  |______________|________________|
C                STARTRUN       T=0              ENDRUN
C
C       2a      IDENTIFY FIRST AND LAST DAILY TIME SERIES
C               ITSDLY                          LTSDLY
C                  |______________|________________|
C               STARTRUN        T=0              ENDRUN
C
C       3.      FIND OBSERVED MAX AND MIN
C                       -ROUND OBSERVED DEPTHS TO NEAREST TENTH OF A FOOT
C                       -IDENTIFY OBSERVED MAX AND MIN
C
C       4.      MATCH OBSERVED MAX/MIN TO PREDICTED
C
C       5.      CALCULATE MAX/MIN BALANCES
C                       OBSERVED - PREDICTED
C
C.      6.      DISPLAY TIDE BALANCE REVIEW
C
C               OBSERVED - PREDICTED = BALANCE      TIME      DATE
C                  8.8        8.4          .4         200     10/16
C                   .4         .2          .2         800     10/16
C                  9.8        9.4          .4        1400     10/16
C                  -.5       -1.3          .8        2100     10/16
C                  8.8        8.3          .5         300     10/17
C
C       7.      TREND FUTURE ASTORIA BALANCES
C
C       8.      DISPLAY BALANCES
C
C               ASTORIA BALANCES:   (E=ESTIMATE)
C
C               DATE  10/14  10/15  10/16  10/17  10/18  10/19  
C        FIRST  TIDE    .0     .0     .2     .2E    .1E    .1E 
C        SECOND TIDE   -.1     .1     .4     .3E    .3E    .2E 
C        THIRD  TIDE    .1     .5     .8     .6E    .5E    .4E 
C        FOURTH TIDE    .3     .4     .5     .4E    .3E    .3E
C
C        9.     CREATE TIME SERIES (OBS and FX MAX/MIN) BALANCES FOR 
C                      TIDE1,2,3,4.. WRITE TO DATABASE FOR 
C                      TSCHANGE OPTION
C
C     VARIABLE DEFINITIONS
C
C     ODEPTH(*)   - DW OBSERVED STAGE TIME SERIES       INPUT
C     PDEPTH(*)   - NOS         STAGE TIME SERIES       INPUT
C     OTS(*)      - TIME MARKER                         CALC INPUT
C       JHR(*)    -       TIDE HOUR
C       JDAY(*)   -       TIDE DAY
C       JMONTH(*) -       TIDE MONTH
C     ODM(*)      - HEIGHT MAX/MIN OBS                  OUTPUT
C     PDM(*)      - HEIGHT MAX/MIN PREDICTED            OUTPUT
C     OTM(*)      - TIME   MAX/MIN MARKER               OUTPUT
C     K1          - BEGIN TIME OF MAXMIN                INPUT
C     K2          - END   TIME OF MAXMIN                INPUT
C     KM          - # OF MAX/MIN POINTS                 OUTPUT
C     BAL(*)      - (OBSERVED - PREDICTED); ODM-PDM
C     DWTIDA(*)   - ADJUSTED TIDE                       OUTPUT
C     ASTID1,2,3,4(*) - OBS/FX  MAX/MIN BALANCES TIDE1,2,3,4  OUTPUT
C-----------------------------------------------------------------------
C     DEBUG COMMON
C        IODBUG - UNIT NUMBER TO WRITE OUT ALL DEBUG OUTPUT in fdbug
C
C
C     UNIT NUMBERS COMMON
C     ALWAYS USE THE VARIABLES IN IONUM TO SPECIFY UNIT NUMBER
C
C
C     TIMING INFORMATION COMMON
C
C
C     IDARUN - I* 4 - INITIAL JULIAN DAY OF THE ENTIRE RUN
C     IHRRUN - I* 4 - INITIAL HOUR OF THE ENTIRE RUN
C     LDARUN - I* 4 - JULIAN DAY OF LAST DAY OF THE ENTIRE RUN
C     LHRRUN - I* 4 - LAST HOUR OF ENTIRE RUN
C     LDACPD - I* 4 - JULIAN DAY OF LAST DAY WITH OBSERVED DATA
C     LHRCPD - I* 4 - LAST HOUR WITH OBSERVED DATA
C     NOW    - I* 4 - CURRENT TIME FROM THE COMPUTER'S CLOCK
C                     NOW(1) - MONTH
C                     NOW(2) - DAY
C                     NOW(3) - YEAR (4 DIGIT)
C     LOCAL  - I* 4 - HOUR OFFSET TO LOCAL TIME
C     NOUTZ  - I* 4 - DEFAULT TIME ZONE NUMBER FOR OUTPUT
C     NOUTDS - I* 4 - DEFAULT DAYLIGHT SAVING TIME SWITCH FOR OUTPUT
C                     =0, STANDARD TIME
C                     =1, DAYLIGHT SAVING TIME
C     NLSTZ  - I* 4 - TIME ZONE NUMBER OF LOCAL STANDARD TIME
C     IDA    - I* 4 - JULIAN DATE OF THE FIRST DAY TO BE COMPUTED
C     IHR    - I* 4 - FIRST HOUR TO BE COMPUTED IN THE CURRENT PASS
C     LDA    - I* 4 - JULIAN DATE OF THE LAST DAY TO BE COMPUTED
C     LHR    - I* 4 - LAST HOUR TO BE COMPUTED IN THE CURRENT PASS
C     IDADAT - I* 4 - JULIAN DATE OF THE FIRST DAY OF TIME SERIES DATA
C
C     CONTROL INFORMATION FOR SAVING CARRYOVER
C
C
C     IFILLC - I* 4 - CONTROLS UPDATE OF C ARRAY AND STORING CARRYOVER
C                     =0, NO OPERATION CAN MODIFY THE C ARRAY; NO
C                         CARRYOVER STORED
C                     =1, C ARRAY SHOULD BE MODIFIED
C     NCSTOR - I* 4 - NUMBER OF CARRYOVER DATES SAVED TO BE SAVED
C                     IGNORED IF IFILLC=0
C     ICDAY  - I* 4 - JULIAN DAYS TO STORE CARRYOVER
C     ICHOUR - I* 4 - HOURS TO STORE CARRYOVER
C-----------------------------------------------------------------------
      SUBROUTINE EX59 (P,ODEPTH,PDEPTH,ASTID1,ASTID2,ASTID3,ASTID4)

      DIMENSION P(*),ODEPTH(*),PDEPTH(*),ASTID1(*),ASTID2(*),ASTID3(*),
     +          ASTID4(*)
      DIMENSION JHR(1200),JDAY(1200),JMONTH(1200),OTS(1200),BAL(1200),
     +          PPDM(1200),PDM(1200),OTM(1200),PTM(1200),ODM(1200)
      CHARACTER *1 EST(1200)

C     COMMON BLOCKS
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fcary'

C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/ofs/src/fcst_ex/RCS/ex59.f,v $
     . $',                                                             '
     .$Id: ex59.f,v 1.5 2007/01/05 15:52:40 xfan Exp $
     . $' /
C    ===================================================================
C
C     CHECK THE TRACE LEVEL AND WHETHER DEBUG OUTPUT IS NEEDED
      CALL FPRBUG ('EX59    ',1,59,IBUG)

C  --- LOCATE FIRST DATA VALUE IN THE 'HOURLY' TIME SERIES DATA ARRAYS
C     
      IBUG= 0
      KDA = IDA
      KHR = IHR
      ITS = (KDA-IDADAT) * 24 + KHR
      ITSDLY = (KDA-IDADAT) + KHR / 24

      IF (IBUG.EQ.1) WRITE(IODBUG,1200) ITS,ITSDLY
 1200 FORMAT(' EX59: ITS ITSDLY : ',2I5)
     
      IF (IBUG.EQ.1) WRITE(IODBUG,1000) P(20),P(21),P(22)
 1000 FORMAT(' EX59: P ARRAY - OBS DEPTH : ',3A)

      IF (IBUG.EQ.1) WRITE(IODBUG,1001) P(23),P(24),P(25)
 1001 FORMAT(' EX59: P ARRAY - NOS DEPTH : ',3A)

C  DEBUG OUTPUT FOR FIRST DATA VALUE LOCATION

      IF (IBUG.EQ.1) WRITE(IODBUG,1002) KDA,KHR,IDADAT,ITS
 1002 FORMAT(' EX59: KDA,KHR,IDADAT,ITS: ',4I6)

C  --- LOCATE LAST DATA VALUE IN THE 'HOURLY' TIME SERIES DATA ARRAYS
C
      LTS = (LDA-(IDADAT-1)) * 24 + KHR - 1

C  --- LOCATE LAST DATA VALUE IN THE 'DAILY' TIME SERIES DATA ARRAYS
C
      LTSDLY = (LDA-(IDADAT-1)) + KHR / 24
      IF (IBUG.EQ.1) WRITE(IODBUG,1210) LTS,LTSDLY
 1210 FORMAT(' EX59: LTS LTSDLY : ',2I5)

C  --- LOCATE LATEST OBS  VALUE IN 'HOURLY' TIME SERIES DATA
C
      LOTS = (LDACPD-IDADAT) * 24 + LHR

C  DEBUG OUTPUT FOR LAST DATA VALUE LOCATION

      IF (IBUG.EQ.1) WRITE(IODBUG,1003) LDA,LTS
 1003 FORMAT(' EX59: LDA,LTS: ',2I6)

C  --- DETERMINE HR, DAY, MONTH AND TIME MARKER FOR TIME SERIES
C
      DO 100 I=ITS,LTS+24
        OTS(I)=I
        CALL MDYH1(KDA,KHR,ILM,ILD,ILY,ILH,NOUTZ,NOUTDS,NTZCD) 
        JHR(I)=ILH
        JMONTH(I)=ILM
        JDAY(I)=ILD
        KHR=KHR+1
        IF (KHR.GT.24) THEN
          KHR=1
          KDA=KDA+1
        ENDIF
      IF (IBUG.EQ.1) WRITE(IODBUG,1005) ILH,ILM,ILD,ITS,LOTS,LTS,I
 1005 FORMAT(' EX59: ILH,ILM,ILD,ITS,LOTS,LTS,I: ',7I6)
  100 CONTINUE
C
C
C  ---  FIND MAX AND MIN OF OBSERVED DATA
C
C       ROUND OBS HEIGHTS TO NEAREST TENTH OF A FOOT
C       SO TIDAL HIGH AND LOW SEARCH ROUTINE DOES NOT WORK WITH
C       HUNDREDTHS OF FEET
C
C              ODEPTH   = HEIGHT       (TID)                  INPUT
C              OTS      = TIME MARKER  AST                    CALC INPUT
C                JHR    = TIDE HOUR
C                JDAY   = TIDE DAY
C                JMONTH = TIDE MONTH
C              ODM      = HEIGHT OBS/FX         MAX OR MIN    OUTPUT
C              PDM      = HEIGHT PREDICTED(NOS) MAX OR MIN    OUTPUT      
C              OTM      = TIME MARKER           MAX OR MIN    OUTPUT
C              K1       = BEGIN TIME OF MAXMIN                INPUT
C              K2       = END   TIME OF MAXMIN                INPUT
C              KM       = # OF MAX/MIN POINTS                 OUTPUT
C              BAL      = MAX/MIN BALANCES
C                           (ODM - PDM)
C
      K1=ITS
      K2=LOTS
      K3=LTS
      CALL MXMN59(ODEPTH,OTS,ODM,OTM,K1,K2,KM)
      IF (IBUG.EQ.1) WRITE(IODBUG,1006) (I,KM,ODEPTH(I),JMONTH(I),
     .           JDAY(I),JHR(I),I=ITS,LOTS)
 1006 FORMAT(' EX59: I,KM,ODEPTH Mon Day Hr: ',2I6,F10.2,3I8)
      IF (IBUG.EQ.1) WRITE(IODBUG,1008) (I,ODM(I),OTM(I),I=1,KM)
 1008 FORMAT(' EX59: I,ODM, OTM: ',I6,F6.1,F6.1)
      
      CALL MXMN59(PDEPTH,OTS,PPDM,PTM,K1,K3,KPM)
C
C       FOLLOWING LINE ADDED TO GET LAST DATE FOR IFP DISPLAY
C
      PTM(KPM+1)=PTM(KPM)+6
      PTM(KPM+2)=PTM(KPM)+6
      PTM(KPM+3)=PTM(KPM)+6
      PTM(KPM+4)=PTM(KPM)+6
      IF (IBUG.EQ.1) WRITE(IODBUG,1007) (I,KPM,PDEPTH(I),JMONTH(I),
     .           JDAY(I),JHR(I),I=ITS,LTS)
 1007 FORMAT(' EX59: I,KPM,PDEPTH Mon Day Hr: ',2I6,F10.2,3I8)
      IF (IBUG.EQ.1) WRITE(IODBUG,1009) (I,PPDM(I),PTM(I),I=1,KPM+4) 
 1009 FORMAT(' EX59: I,PPDM, PTM: ',I6,F6.1,F6.1)

C
C  ---   MATCH PREDICTED TIDE TO OBSERVED TIDE AT OBSERVED MAX/MIN
C          . FIND THE HEIGHT OF PREDICTED TIDE AT OBSERVED MAX/MIN
C
C  ---   COMPUTE BALANCES (ENGLISH)
C
C
      DO 150 I=1,KM
        EST(I)=' '
  150 CONTINUE
  
      DO 200 I=1,KM
        PDM(I)=PDEPTH( INT(OTM(I)) )
        BAL(I)=ODM(I)-PDM(I)
        BAL(I)=BAL(I)*3.281

  200 CONTINUE
C
C ---  ROUND ENGLISH BALANCES 
C      

      CALL RND59(BAL,1,KM)

C
C  ---   DISPLAY BACKUP HIGHS AND LOWS
C
C
        WRITE(IPR,260)
  260 FORMAT(/,' OBSERVED - PREDICTED = BALANCE    TIME      DATE')
      DO 250 I=1,KM
        INTOTM = INT( OTM(I) )
        WRITE(IPR,265) ODM(I)*3.281,EST(I),PDM(I)*3.281,BAL(I),EST(I),
     ?               JHR(INTOTM),JMONTH(INTOTM),JDAY(INTOTM)
  265 FORMAT(1X,F5.1,1A1,5X,F5.1,7X,F5.1,1A1,7X,I2,'00',5X,I2,'/',I2)
  250 CONTINUE
C
C  ---   TREND FUTURE BALANCES
C
C     KPME EXTENDED TO GET LAST DATE FOR IFP DISPLAY
C
      KPME=((KPM/4 + 2)*4)
      DO 300 K=KM+1,KPME
         BAL(K)=BAL(K-4)*0.8
         EST(K)='E'
  300 CONTINUE

C
C  ---   CREATE TID1,2,3,4 (BACKUP & FUTURE BALANCES) IN DATABASE 
C           FOR LATER TSCHANGE
      IK=ITSDLY
      DO 245 I=1,KPME,4
         ASTID1(IK)=BAL(I)
         ASTID2(IK)=BAL(I+1)
         ASTID3(IK)=BAL(I+2)
         ASTID4(IK)=BAL(I+3)
         IK=IK+1
  245 CONTINUE
      IF (IBUG.EQ.1) WRITE(IODBUG,1010) IK,KPM,KPME
 1010 FORMAT(' EX59: IK, KPM, KPME: ',I6,I6,I6)
      WRITE(IPR,600)
  600 FORMAT(//,'ASTORIA BALANCES:  ',/)
      IF (IK.LE.10) THEN
      WRITE(IPR,605)
     $  (JMONTH( INT(PTM(J)) ),JDAY( INT(PTM(J)) ),J=1,KPME,4)
  605 FORMAT(8X,'DATE',10(2X,I2,'/',I2))
        WRITE(IPR,610) (ASTID1(J),EST((J-ITSDLY+1)*4-3),J=ITSDLY,IK-1)
        WRITE(IPR,620) (ASTID2(J),EST((J-ITSDLY+1)*4-2),J=ITSDLY,IK-1)
        WRITE(IPR,630) (ASTID3(J),EST((J-ITSDLY+1)*4-1),J=ITSDLY,IK-1)
        WRITE(IPR,640) (ASTID4(J),EST((J-ITSDLY+1)*4),J=ITSDLY,IK-1)
  610 FORMAT(1X,'FIRST  TIDE ',10(F5.1,1A1,1X))
  620 FORMAT(1X,'SECOND TIDE ',10(F5.1,1A1,1X))
  630 FORMAT(1X,'THIRD  TIDE ',10(F5.1,1A1,1X))
  640 FORMAT(1X,'FOURTH TIDE ',10(F5.1,1A1,1X)/)
      ELSE 
       WRITE(IPR,605)
     $   (JMONTH( INT(PTM(J)) ),JDAY( INT(PTM(J)) ),J=1,37,4)
       WRITE(IPR,610) (ASTID1(J),EST((J-ITSDLY+1)*4-3),J=ITSDLY,
     +                 ITSDLY+9)
       WRITE(IPR,620) (ASTID2(J),EST((J-ITSDLY+1)*4-2),J=ITSDLY,
     +                 ITSDLY+9)
       WRITE(IPR,630) (ASTID3(J),EST((J-ITSDLY+1)*4-1),J=ITSDLY,
     +                 ITSDLY+9)
       WRITE(IPR,640) (ASTID4(J),EST((J-ITSDLY+1)*4),J=ITSDLY,
     +                 ITSDLY+9)
       IF (IK.LE.20) THEN
        WRITE(IPR,605)
     $    (JMONTH( INT(PTM(J)) ),JDAY( INT(PTM(J)) ),J=41,KPME,4)
        WRITE(IPR,610) (ASTID1(J),EST((J-ITSDLY+1)*4-3),J=ITSDLY+10,
     +                  IK-1)
        WRITE(IPR,620) (ASTID2(J),EST((J-ITSDLY+1)*4-2),J=ITSDLY+10,
     +                  IK-1)
        WRITE(IPR,630) (ASTID3(J),EST((J-ITSDLY+1)*4-1),J=ITSDLY+10,
     +                  IK-1)
        WRITE(IPR,640) (ASTID4(J),EST((J-ITSDLY+1)*4),J=ITSDLY+10,
     +                  IK-1)
       ELSE 
         WRITE(IPR,605)
     $     (JMONTH( INT(PTM(J)) ),JDAY( INT(PTM(J)) ),J=41,77,4)
         WRITE(IPR,610) (ASTID1(J),EST((J-ITSDLY+1)*4-3),J=ITSDLY+10,
     +                   ITSDLY+19)
         WRITE(IPR,620) (ASTID2(J),EST((J-ITSDLY+1)*4-2),J=ITSDLY+10,
     +                   ITSDLY+19)
         WRITE(IPR,630) (ASTID3(J),EST((J-ITSDLY+1)*4-1),J=ITSDLY+10,
     +                   ITSDLY+19)
         WRITE(IPR,640) (ASTID4(J),EST((J-ITSDLY+1)*4),J=ITSDLY+10,
     +                   ITSDLY+19)
         WRITE(IPR,605)
     $     (JMONTH( INT(PTM(J)) ),JDAY( INT(PTM(J)) ),J=81,KPME,4)
         WRITE(IPR,610) (ASTID1(J),EST((J-ITSDLY+1)*4-3),J=ITSDLY+20,
     +                    IK-1)
         WRITE(IPR,620) (ASTID2(J),EST((J-ITSDLY+1)*4-2),J=ITSDLY+20,
     +                    IK-1)
         WRITE(IPR,630) (ASTID3(J),EST((J-ITSDLY+1)*4-1),J=ITSDLY+20,
     +                    IK-1)
         WRITE(IPR,640) (ASTID4(J),EST((J-ITSDLY+1)*4),J=ITSDLY+20,
     +                    IK-1)
       END IF
      END IF
C
C ---  CONVERT ENG BALANCES BACK TO METRIC
C
      IK=ITSDLY
      DO 650 I=1,KPME,4
       ASTID1(IK)=ASTID1(IK)/3.281
       ASTID2(IK)=ASTID2(IK)/3.281
       ASTID3(IK)=ASTID3(IK)/3.281
       ASTID4(IK)=ASTID4(IK)/3.281
       IK=IK+1
  650 CONTINUE
      END
