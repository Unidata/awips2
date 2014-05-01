C     MODULE  FCEX43      VERSION 1.1      
c
c  ====================================================================
c  pgm:  ex43 (po,co,px,ro,apis,atis,ris)
c
c   in: po     .... parameter array
c   in: co     .... carryover array
c   in: px     .... rainfall + melt time series
c  out: ro     .... runoff time series
c  out: apis   .... antecedent precipitation index time series - option
c  out: atis   .... antecedent temperature index time series - option
c  out: ris    .... runoff index (ai) time series - option
c  ====================================================================
      SUBROUTINE EX43(PO,CO,PX,RO,APIS,ATIS,RIS)

C######################################################################

C  THIS ROUTINE EXECUTES THE API-HFD OPERATION.

C######################################################################
C  LIST OF VARIABLES:
C  NAME    DEFINITION                                 S/D  I/O  UNITS
C  ______  _________________________________________  ___  ___  ________
C  AEX     EXPONENT FOR FORMULA IN DETERMINING RI2
C  ALINE   ATI OR PRECIPITATION CURVE LINES
C  AI      ANTECEDENT INDEX (QUAD I)
C  AM      SLOPE COMPONENT IN RI2 EQUATION (QUAD II)
C  API     FCST LOCATION ANTECEDENT PREC. INDEX
C  ATI     FCST LOCATION ANTECEDENT TEMPERATURE INDEX
C  ATIS    ATI TIME SERIES (IF REQUESTED)             TS    O   DEG FAHR
C  APIS    API TIME SERIES (IF REQUESTED)             TS    O   MM
C  ATIX    ARRAY FOR TEMPORARY STORAGE OF ATI VALUES   L        DEG FAHR
C  APIX    ARRAY FOR TEMPORARY STORAGE OF API VALUES   L        INCHES
C  AX      SUMMER OR WINTER PARAMETER SLOPE MULTIPLIER
C  BLIMS   MIN ATI VALUE ON SUMMER API/ATI/RI CURVE    P    I   DEG FAHR
C  BLIMW   MIN ATI VALUE ON WINTER API/ATI/RI CURVE    P    I   DEG FAHR
C  CAPI    API VALUE AT 12Z TODAY                           O   INCHES
C  CARY    ARRAY FOR TEMPORARY STORAGE OF CARRYOVER    L
C  DRAIM   24 HOUR (DAILY) RAIN/MELT    C                   O   INCHES
C  DRO     24 HOUR (DAILY) RUNOFF       C                   O   INCHES
C  FACTOR  RO FACTOR USED IN RO SUBROUTINE            SUB
C  IATI    ANTECEDENT INDEX AT INFLECTION POINT
C  ICAPI   API MOD CORRECTION COUNTER                  L
C  ICDAY   ARRAY OF DATES TO SAVE CARRYOVER           CM    I   JDAYS
C  IDADAT  FIRST DAY OF DATA IN TIME SERIES           CM    I   JDAYS
C  IDAY    FIRST DAY OF RUN        CM                       I   JDAYS
C  IDELTA  COMPUTATIONAL TIME STEP INTERVAL            P    I   HOURS
C  IDT29   ARRAY OF DATES FOR API CORRECTIONS         CM    I   JHOURS
C  IERR    ERROR CODE
C  IFDEB   DEBUG OUTPUT SWITCH (0 OR 1)               SUB   I
C  IFF     NUMBER OF DAYS IN A GIVEN MONTH
C  IFFG    FLASH FLOOD GUIDANCE SWITCH                CM    I
C          0 = NO FFG, 1 = FFG ONLY, 2 = FFG & FCST
C  IFILLC  SAVE CARRYOVER SWITCH (0 OR 1)             CM    I
C  IMIN    CLOCK MINUTE
C  INTDA   INTERNAL DAY COUNTER                        L        DAYS
C  INTHR   INTERNAL HOUR COUNTER                       L        HOURS
C  IOFAAA  API/ATI/RI TIME SERIES OUTPUT FLAG          P    I
C  IRNUM   RUNOFF ZONE NUMBER                          P    I
C  IT      LOCAL HOUR ON 24 HOUR CLOCK
C  IUSEC   NUMBER OF WORDS USED IN THE CO ARRAY        P    I
C  KDA     TODAYS DAY                                  L
C  KDAFFG  COMPUTATIONAL DAY FOR FFG OUTPUT            L
C  KHRFFG  COMPUTATIONAL HOUR FOR FFG OUTPUT           L
C  KIATI   DATA FOR API/ATI CURVE                     SUB   I
C  KINDX   AI CURVE INFLECTION POINTS
C  KIRI2   DATA FOR RI/RO PRECIPITATION CURVE         SUB   I
C  KMO     TODAYS MONTH                                L
C  KYR     TODAYS YEAR                                 L
C  LAST    # OF DAYS FOR WHICH OPERATION IS TO BE RUN  L
C  LDA     LAST DAY OF RUN             CM                   I   JDAYS
C  LEN     LENGTH OF TABLE (ENDING POSITION OF TABLE)
C  LINDX   RO CURVE INFLECTION POINTS
C  LOC     ARRAY LOCATION FOR THE CURRENT PERIODS      L
C          RAIM, RO, RI, API & ATI TIME SERIES DATA
C  LOCATI  ARRAY LOCATION FOR THE CURRENT DAYS ATI     L
C          TIME SERIES DATA
C  MATI    WEEKLY BASIN TEMPERATURES                   P    I
C  NATIBS  NUMBER OF TIMES ATI HAS GONE BELOW LOWER    L
C          LIMIT (SUMMER CURVE)
C  NATIBW  NUMBER OF TIMES ATI HAS GONE BELOW LOWER    L
C          LIMIT (WINTER CURVE)
C  NATITS  NUMBER OF TIMES ATI HAS TOPPED THE UPPER    L
C          LIMIT (SUMMER CURVE)
C  NATITW  NUMBER OF TIMES ATI HAS TOPPED THE UPPER    L
C          LIMIT (WINTER CURVE)
C  NAPIT   NUMBER OF TIMES API HAS TOPPED UPPER LIMIT  L
C          (NOW DEFINED AS 5.00)
C  NAPIB   NUMBER OF TIMES API HAS DROPPED BELOW       L
C          LOWER LIMIT (NOW DEFINED AS 0.10)
C  NCSTOR  NUMBER OF DAYS TO STORE CARRYOVER          CM    I
C  NDA     TOMORROWS DAY                               L
C  NDT29   NUMBER OF API CORRECTIONS TO BE MADE       CM    I
C  NMO     TOMORROWS MONTH                             L
C  NOC     SAVE CARRYOVER COUNTER                      L
C  NOP     NUMBER ASSIGNED TO THIS OPERATION           P    I
C  NPER    NUMBER OF PERIODS IN A DAY                  P    I
C  NREL    NUMBER OF API/ATI/RO RELATIONSHIP           P    I
C  NSW     NEW STORM WINDOW                            P    I   HOURS
C  NSPER   NUMBER OF PERIODS IN NSHR                   P    I
C  NWD1    NUMBER OF WORDS USED IN PX, RO, ATIS,       L
C          APIS AND RIS TIME SERIES
C  NWD2    NUMBER OF WORDS USED IN PE TIME SERIES      L
C  NYR     TOMORROWS YEAR                              L
C  PMAX    MAX RAIN/MELT ALLOWED (WITHIN NSW) FOR      P    I
C          A NEW STORM TO BEGIN
C  PREC    6 HOUR PRECIPITATION VALUE FOR RO ROUTINE        I   INCHES
C  PX      RAIN/MELT TIME SERIES                      TS    I   MM
C  PXX     ARRAY FOR TEMPORARY STORAGE OF RAIN/MELT    L        INCHES
C  R24     24 HOUR API RECESSION COEFFICIENT           P    I
C  REC     INCREMENTAL API RECESSION COEFFICIENT       L
C  RFCTR   RUNOFF ADJUSTMENT FACTOR                    P    I
C  RIS     RI TIME SERIES (IF REQUESTED)              TS    O
C  RIX     ARRAY FOR TEMPORARY STORAGE OF RI VALUES    L
C  RI2     FINAL INDEX VALUE (QUAD II)
C  RID     RUNOFF ZONE ID                              P    I
C  RLAT    LATITUDE OF RUNOFF ZONE CENTROID            P    I   DEG DEC
C  RLNG    LONGITUDE OF RUNOFF ZONE CENTROID           P    I   DEG DEC
C  RNAME   RUNOFF ZONE NAME                            P    I
C  RNS     TOTAL RAIN/MELT WITHIN NEW STORM WINDOW     L        INCHES
C  RNSP    RAIN/MELT FOR EACH PERIOD WITHIN NEW        C    O   INCHES
C          STORM WINDOW
C  RO      GENERATED RUNOFF TIME SERIES               TS    O   MM
C  ROX     ARRAY FOR TEMPORARY STORAGE OF RUNOFF       L        INCHES
C  RUNOFF  GENERATED RUNOFF USING TOTAL STORM          L        INCHES
C          RAIN/MELT
C  SATI    CURRENT STORM ATI VALUE                         I,O  DEG FAHR
C  SAPI    CURRENT STORM API VALUE                         I,O  INCHES
C  SRI     CURRENT STORM RI VALUE                          I,O
C  SRAIM   CURRENT STORM RAIN/MELT                         I,O  INCHES
C  SRO     CURRENT STORM RUNOFF                            I,O  INCHES
C  TAPI    API VALUE FOR THE CURRENT PERIOD            L        INCHES
C  TATI    ATI VALUE AT 12Z TODAY                           O   DEG FAHR
C  TBAR    DERIVED AVERAGE YEARLY BASIN TEMP                I   DEG FAHR
C  TTBAR   CORRECTED AVERAGE YEARLY BASIN TEMP              O   DEG FAHR
C  TRI     RI VALUE AT 12Z TODAY                            O
C  ULIMS   MAX ATI VALUE ON SUMMER API/ATI/RI CURVE    P    I   INCHES
C  ULIMW   MAX ATI VALUE ON WINTER API/ATI/RI CURVE    P    I   INCHES
C  VAL29   ARRAY OF API CORRECTIONS    CM                   I   INCHES
C  IWENO   WEEK NUMBER 1 - 52          CM              L   I,O
C  YATI    YESTERDAYS 12Z ATI VALUE     C                   I   INCHES
C  YAPI    YESTERDAYS 12Z API VALUE     C                   I   INCHES
C  YRI     YESTERDAYS 12Z RI VALUE      C                   I
C
C                      ABBREVIATIONS:
C
C                         S/D = SOURCE/DESTINATION
C
C                            TS = TIME SERIES
C                            C  = CARRYOVER
C                            P  = PARAMETRIC
C                            L  = LOCAL
C                            SUB= SUBROUTINE
C                            CM = COMMON BLOCK
C
C                         I/O = INPUT/OUTPUT
C
C                            I  = INPUT
C                            O  = OUTPUT
C
C#######################################################################
c  Initially written by
c     Ken Mack  NERFC                                     Sep 25, 1995
c     Tim Sweeney HRL                                     Oct 1995
c 
c  Corrected time increment and week number calculation
c     Tim Sweeney, HRL                                    Jan 10, 1997 
c.......................................................................
c
      include 'common/fclfls'
      include 'common/fdbug'
      include 'common/fctime'
      include 'common/fcary'
      include 'common/ffgctl'
      include 'common/ionum'
c.......................................................................
c
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c      COMMON /FCTIME/ IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
c     1                NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,
c     2                LDA,LHR,IDADAT
c      COMMON /FCARY/ IFILLC,NCSTOR,ICDAY(20),ICHOUR(20)
c      COMMON/IONUM/ IN,IPR,IPU
      common /tblbug/ ikh,nrelh
      COMMON /MOD129/ NDT29,IDT29(5),VAL29(5)
c.......................................................................
c
      DIMENSION PO(*),CO(*),PX(*),RO(*),RIS(*),APIS(*),ATIS(*),
     1          CARY(34),RNAME(5),RID(2),SUBNAM(2),MODA(12),
     2          PXX(24),ROX(24),RIX(24),APIX(24),ATIX(24)
      DIMENSION RNSP(24),MATI(52)
c
      INTEGER*4 IWENO,NATI
      INTEGER*4 JHR,IMIN,ISEC,IYR,IMON,IDAY,JDAY
      INTEGER*4 IER,IRSV,KHR,IJUL,KYR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ex43.f,v $
     . $',                                                             '
     .$Id: ex43.f,v 1.3 1997/01/29 21:06:28 page Exp $
     . $' /
C    ===================================================================
C
c.......................................................................
c
      DATA SUBNAM /4HEX43,4H    /,NOP/43/
      DATA MODA /31,28,31,30,31,30,31,31,30,31,30,31/
c.......................................................................
c
C  CALL DEBUG CHECK ROUTINE
c  Trace level=1, Debug flag=IFDEB
c
      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)
c
c================================================================
c
C  INITIALIZE COUNTER FOR THE NUMBER OF DAYS CARRYOVER
C  IS BEING SAVED.  THIS COUNTER IS INCREMENTED BY 1 EACH
C  DAY CARRYOVER IS SAVED.  ALSO, INITIALIZE
C  THE COUNTERS FOR THE ATI AND API MOD CORRECTIONS, AND FOR
C  ATI AND API ERROR COUNTERS.
c
c================================================================
      NOC   = 1
      ICATI = 1
      ICAPI = 1
c
      natibs = 0
      natibw = 0
      natits = 0
      natitw = 0
      napit  = 0
      napib  = 0
c
      ikh = 0
      nrelh = 0
c
c=========================================================
C  SELECT REQUIRED VARIABLES FROM THE PO AND CO ARRAYS.
c=========================================================

      VERS   = PO(1)
      RID(1) = PO(2)
      RID(2) = PO(3)
      DO 100 I=1,5
100   RNAME(I)=PO(I+3)
      IRNUM  = PO(9)
      RLAT   = PO(10)
      RLNG   = PO(11)
      RFCTR  = PO(12)
      R24    = PO(13)
      PMAX   = PO(14)
      ULIMW  = PO(15)
      BLIMW  = PO(16)
      ULIMS  = PO(17)
      BLIMS  = PO(18)
      TBAR   = PO(19)
      TTBAR  = PO(20)
      NREL   = PO(23)
      IDELTA = PO(24)
      NSW    = PO(25)
      NSPER  = PO(26)
      IUSEC  = PO(27)
      IOFAAA = PO(28)
      LMTS   = PO(32)
      LWKT   = PO(33)
      LOTS   = PO(34)
C
      DO 110 I=1,52
110   MATI(I) = PO(LWKT+I-1)
c
      YAPI   = CO(1)
      YATI   = CO(2)
      YRI    = CO(3)
      SAPI   = CO(4)
      SATI   = CO(5)
      SRI    = CO(6)
      SRAIM  = CO(7)
      SRO    = CO(8)
      draim  = co(9)
      dro    = co(10)

      DO 125 I=1,NSPER
125   RNSP(I) = CO(10+I)
      NPER = 24/IDELTA
C
      IF(IFDEB.GT.0) WRITE(IODBUG,8050)SUBNAM,VERS
8050  FORMAT(/5X,2A4,'DEBUG OUTPUT.',5X,'VERSION: ',f4.2)

C  CALCULATE THE NUMBER OF WORDS USED IN THE TIME SERIES
C                 PASSED INTO THIS ROUTINE.

      NWD1 = (LDA-IDADAT+1)*24/IDELTA
      NWD2 = (LDA-IDADAT+1)
C
      IF(IFDEB.GT.0) THEN
4060    WRITE(IODBUG,8060)
8060    FORMAT(//,8X,'THE FOLLOWING TIME VARIABLES WERE RECEIVED BY ',
     1  'THE EX43 ROUTINE',/,8x,'VIA THE FCTIME COMMON BLOCK:',
     2  //,5X,'NAME',6X,'DESCRIPTION',50X,'DAY/HOUR')
        WRITE(IODBUG,8061) IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD
        WRITE(IODBUG,8067) NOW,LOCAL,NOUTZ,NOUTDS,NLSTZ
        WRITE(IODBUG,8076) IDA,IHR,LDA,LHR,IDADAT
8061  FORMAT(5X,'IDARUN    INITIAL DAY OF THE ENTIRE RUN',33X,I6,
     1      /,5X,'IHRRUN    INITIAL HOUR OF THE ENTIRE RUN (INTERNAL ',
     2       'CLOCK)',15X,I6,
     3      /,5X,'LDARUN    LAST DAY OF THE ENTIRE RUN',36X,I6,
     4      /,5X,'LHRRUN    LAST HOUR OF THE ENTIRE RUN (INTERNAL ',
     5        'CLOCK)',18X,I6,
     6      /,5X,'LDACPD    LAST DAY WITH OBSERVED DATA',35X,I6,
     7      /,5X,'LHRCPD    LAST HOUR WITH OBSERVED DATA (INTERNAL ',
     8        'CLOCK)',17X,I6)
8067  FORMAT(5X,'NOW(1)    CURRENT MONTH FROM COMPUTERS CLOCK',30X,I4,
     1     /,5X,'NOW(2)    CURRENT DAY FROM COMPUTERS CLOCK',32X,I4,
     2     /,5X,'NOW(3)    CURRENT YEAR FROM COMPUTERS CLOCK',31X,I4,
     3     /,5X,'NOW(4)    CURRENT HOUR AND MINUTE FROM COMPUTERS ',
     4       'CLOCK',20X,I4,
     5     /,5X,'NOW(5)    CURRENT SECOND AND MILLISECOND FROM ',
     6       'COMPUTERS CLOCK',13X,I4,
     7     /,5X,'LOCAL     HOUR OFFSET TO LOCAL TIME',39X,I4,
     8     /,5X,'NOUTZ     DEFAULT TIME ZONE NUMBER',40X,I4,
     9     /,5X,'NOUTDS    DEFAULT DAYLIGHT SAVINGS TIME SWITCH',
     1       28X,I4,
     2    /,5X,'NLSTZ     TIME ZONE NUMBER OF LOCAL STANDARD TIME ',
     3       24X,I4)
8076  FORMAT(5X,'IDA       FIRST DAY TO BE COMPUTED IN CURRENT PASS ',
     1    'THROUGH THE ',9X,I6,/,17X,'OPERATIONS TABLE',
     2   /,5X,'IHR       FIRST HOUR TO BE COMPUTED IN CURRENT PASS ',
     3    'THROUGH THE',11X,I4,/,17X,'OPERATIONS TABLE (INTERNAL ',
     4    'CLOCK)',
     5   /,5X,'LDA       LAST DAY TO BE COMPUTED IN CURRENT PASS ',
     6    'THROUGH THE',11X,I6,/,17X,'OPERATIONS TABLE (INTERNAL ',
     7    'CLOCK)',
     8   /,5X,'LHR       LAST HOUR TO BE COMPUTED IN CURRENT PASS ',
     9    'THROUGH THE',10X,I6,/,17X,'OPERATIONS TABLE (INTERNAL ',
     1    'CLOCK)',
     2   /,5X,'IDADAT    FIRST DAY OF TIME SERIES DATA',33X,I6)
      WRITE(IODBUG,8090)
8090  FORMAT(////8X,'THE FOLLOWING TIME SERIES DATA WAS RECEIVED BY ',
     1 'THE EX43 ROUTINE:',//31X,'PERIOD',5X,'PX',/,
     2 38X,'(INCHES)',/)
      DO 4095 I=1,NWD1
      X1 = PX(I)/25.4
4095  WRITE(IODBUG,8095)I,X1
8095  FORMAT(32X,I4,4X,F5.2,7X,F7.2,3(6X,F7.2))
      WRITE(IODBUG,8150)
8150  FORMAT(////8X,'THE FOLLOWING PARAMETRIC DATA WAS RECEIVED BY ',
     1'THE EX43 ROUTINE:')
      WRITE(IODBUG,8151) RID,RNAME,IRNUM,RLAT,RLNG
8151  FORMAT(/34X,'RID   = ',2A4,/34X,'RNAME = ',5A4,
     1/34X,'IRNUM = ',I4,/34X,'RLAT  = ',F5.2,
     2/34X,'RLNG  = ',F5.2)
      WRITE(IODBUG,8152)RFCTR,R24,PMAX,ULIMW,BLIMW,ULIMS,BLIMS,TTBAR
8152  FORMAT(/34X,'RFCTR = ',F5.3,/34X,'R24   = ',F5.3,
     1/34X,'PMAX  = ',F5.3,/34X,'ULIMW = ',F4.1,
     2/34X,'BLIMW = ',F4.1,/34X,'ULIMS = ',F4.1,
     3/34X,'BLIMS = ',F4.1,/34X,'TTBAR = ',F4.1)
      WRITE(IODBUG,8153) NREL,IDELTA,NSW,NSPER,IUSEC,IOFAAA
8153  FORMAT(
     1/34X,'NREL  = ',I4,/34X,'IDELTA= ',I4,
     2/34X,'NSW   = ',I4,/34X,'NSPER = ',I4,
     3/34X,'IUSEC = ',I4,/34X,'IOFAAA= ',I4)
      WRITE(IODBUG,8154) (MATI(I),I=1,52)
8154  FORMAT(//,8X,'WEEKLY BASIN TEMPERATURES:',
     1      4(/,5X,13(1X,I3) ) )
c
      WRITE(IODBUG,8155)
8155  FORMAT(////9X,'THE FOLLOWING CARRYOVER DATA WAS RECEIVED ',
     1'BY THE EX43 ROUTINE:')
      WRITE(IODBUG,8156)YAPI,YATI,YRI,SAPI,SATI,SRI,SRAIM,SRO,DRAIM,DRO
8156  FORMAT(/33X,'YAPI  = ',F5.2,/33X,'YATI  = ',F4.1,
     1/33X,'YRI   = ',F5.2,/33X,'SAPI  = ',F5.2,
     2/33X,'SATI  = ',F4.1,/33X,'SRI   = ',F5.2,
     3/33X,'SRAIM = ',F5.2,/33X,'SRO   = ',F5.2,
     4/33X,'DRAIM = ',F5.2,/33X,'DRO   = ',F5.2)
      WRITE(IODBUG,8157)
8157  FORMAT(/34X,'I',4X,'RNSP(I)')
      WRITE(IODBUG,8158)(I,RNSP(I),I=1,NSPER)
8158  FORMAT( 24(32X,I4,4X,F5.2,/) )
c
      WRITE(IODBUG,8170)
8170  FORMAT(////13X,'THE FOLLOWING CARRYOVER SAVE PARAMETERS WERE ',
     1 'RECEIVED',/,13X,'BY THE EX43 ROUTINE VIA THE FCARY ',
     2 'COMMON BLOCK:')
      WRITE(IODBUG,8171)IFILLC,NCSTOR
8171  FORMAT(33X,'IFILLC = ',I4,/33X,'NCSTOR = ',I4,/)
      WRITE(IODBUG,8172)
8172  FORMAT(27X,'I    ICDAY(I)   ICHOUR(I)')
      DO 4175 I=1,10
4175  WRITE(IODBUG,8175)I,ICDAY(I),ICHOUR(I)
8175  FORMAT(25X,I4,4X,I6,5X,I6)
c
      WRITE(IODBUG,8180)
8180  FORMAT(////17X,'THE FOLLOWING API MOD PARAMETERS WERE RECEIVED ',
     1 /,17X,'BY THE EX43 ROUTINE VIA THE MOD129 COMMON BLOCK:')
      WRITE(IODBUG,8181)NDT29
8181  FORMAT(34X,'NDT29 = ',I4)
      WRITE(IODBUG,8182)
8182  FORMAT(27X,'I    IDT29(I)    VAL29(I)')
      DO 4185 I=1,5
 4185 WRITE(IODBUG,8185)I,IDT29(I),VAL29(I)
8185  FORMAT(25X,I4,4X,I6,6X,F5.2)
      ENDIF
c
c
c====================================================================
c
C  CALL ROUTINE TO GET MONTH, DAY AND YEAR FOR FIRST DATE
C  BEING RUN.  SET THE NUMBER OF DAYS FOR THE RUN AND
C  INITIALIZE THE INTERNAL DAY AND THE INTERNAL HOUR.
c
c====================================================================
      CALL MDYH1(IDA,IHR,KMO,KDA,KYR,KHR,NOUTZ,NOUTDS,TZ)
      KYRC  = KYR
      KYR   = KYR-((KYR/100)*100)
      IF((KMO.EQ.2).AND.(((KYR/4)*4).EQ.KYR)) MODA(2) = 29
      LAST  = LDA-IDA+1
      INTDA = IDA
      INTHR = IHR 
c
c===============================================================
c
C  BEGIN FLASH FLOOD GUIDANCE
C  CALCULATE COMPUTATIONAL PERIOD AT OR JUST BEFORE LSTCMPDY
c
c===============================================================
      IF(IFDEB .GT. 0) WRITE(IODBUG,8230) IFFG
8230    FORMAT(//,10X,'FLASH FLOOD GUIDANCE SWITCH= ',I4)
C
      IF(IFFG .GT. 0) THEN
        KDAFFG = LDACPD
        KHRFFG = (LHRCPD/IDELTA) * IDELTA
        IF(KHRFFG .GT. 0) GOTO 192
        KDAFFG = KDAFFG - 1
        KHRFFG = 24
C
192     IF(IFDEB .GT. 0) WRITE(IODBUG,8240) KDAFFG,KHRFFG
8240      FORMAT(15X,'FFG COMPUTATIONAL DAY=',I6,/,
     1           15X,'FFG COMPUTATIONAL HOUR= ',I4)
      ENDIF
c
C==============  END FLASH FLOOD GUIDANCE ======================
c
c===============================================================
c
C  BEGIN DAILY LOOP
c=======================

      DO 500 II=1,LAST

C  DETERMINE THE LOCATION OF THE FIRST VALUE NEEDED IN THE
C  PX ARRAY, AND THE FIRST VALUE TO BE OUTPUT IN THE
C  RO (AND THE RIS, APIS AND ATIS ARRAYS IF REQUESTED).

      LOC = (INTDA-IDADAT)*NPER+1

C  DETERMINE THE LOCATION OF TODAYS POTENTIAL EVAPOTRANSPIRATION
C  VALUE IN THE API ARRAY.  REMEMBER, THIS IS A 24-HOUR TIME SERIES.
cew ATIS TS use this pointer also

      LOCATI = (INTDA-IDADAT)+1
c
c================================================================
C  CONVERT TODAYS NPER PERIODS OF RAIN/MELT FROM METRIC
C                 INTO ENGLISH.
c================================================================
      DO 255 I=1,NPER
      J = LOC+I-1
      PXX(I) = PX(J)/25.4
255   CONTINUE
      DRAIM = 0.0
      DRO   = 0.0
C
      IF(IFDEB.GT.0) THEN
C4260  WRITE(IODBUG,8260) IDAY,IWENO,LOC,LOCATI,ISEA
C8260  FORMAT(/,1X,78('-'),/,1X,'DAY OF YEAR:',I4,4X,'WEEK:',I4,
C     1       4X,'LOC:',I4,4X,'LOCATI:',I4,4X,'ISEA:',I4)  
       WRITE(IODBUG,8260) RID,KMO,KYRC,TZ,LOC,LOCATI
8260   FORMAT(/,1X,79('-'),/,' API-HFD OUTPUT FOR ',2A4,
     1       4X,I2,1H/,I4,3X,'TIME ZONE=',A4,2X,'LOC:',I4,
     2       3X,'LOCATI:',I4)

      WRITE(IODBUG,8262)
8262  FORMAT(/,' PER DA HR  INTDA INTHR   PXX  TAPI   TATI  ',
     1         ' TAI   SAPI   SATI   SAI    SRI   SRAIM   SRO   ',
     2         ' ROX   IDAY  WK  ISEA',
     3       /,' === == ==  ===== =====  ====  ====  =====  ',
     4         '=====  ====  =====  =====  =====  =====  =====  ',
     5         '=====  ====  ==  ====')
      ENDIF

c================================================================
c
C  LOOP THROUGH EACH PERIOD OF THE DAY.
c
c================================================================
c
      DO 400 I=1,NPER
c
c=================================
C  UPDATE TODAYS TOTAL RAIN/MELT.
c=================================
      DRAIM = DRAIM + PXX(I)
c
c====================================
C  UPDATE THE API FOR THIS PERIOD.
c====================================
      RP  = (I*IDELTA)/24.
      REC = R24**RP

      CAPI=YAPI*REC+DRAIM
c
c=======================================================================
C  CHECK FOR API CORRECTION TO BE APPLIED TO CURRENT COMPUTATIONAL HOUR.
C  IF ALL CORRECTIONS PASSED IN COMMON BLOCK MOD129 HAVE
C  BEEN CHECKED, OR IF CORRECTION IS FOR FORECAST DAY, SKIP CHECK.
c=======================================================================
       IF(ICAPI.GT.NDT29) GOTO 268
       IF(IDT29(ICAPI).GT.((LDACPD-1)*24+LHRCPD)) GOTO 268
       IF(IDT29(ICAPI).NE.((INTDA-1)*24+INTHR)) GOTO 268
       CAPI = VAL29(ICAPI)
       ICAPI= ICAPI+1
c
c==============================================
C  RETROFIT YAPI BASED ON THIS MOD CORRECTION
c==============================================
      YAPI = (CAPI-DRAIM)/REC
c
c====================================================
C  CAPI MUST LIE BETWEEN 0.10 AND 5.00 INCLUSIVELY.
c====================================================
268   IF(CAPI.GT.5.00) THEN
        NAPIT = NAPIT + 1
        CAPI = 5.00
      ELSE IF(CAPI.LT.0.10) THEN
        NAPIB = NAPIB + 1
        CAPI = 0.10
      ELSE
      ENDIF
      TAPI=CAPI
      SAPI=CAPI
c
c==============================================================
C DETERMINE THE WEEK NUMBER
c==============================================================
      iday = 0
      do 227 mm=1,kmo
 227  iday = iday + moda(mm)
      iday = iday - moda(kmo) + kda
      iweno = ((iday-1)/7) + 1
      if(iweno.gt.52) iweno = 1
c
CCCCCCCCCCCCC  DETERMINE PARAMETERS FOR QUADRANT II CCCCCCCCCCCCCCCCCCCC
      IF ((IWENO .GT. 12) .AND. (IWENO .LE. 37)) THEN
C        SUMMER CURVES
         AX   = 0.721
         AEX  = 0.221

C        WINTER CURVES
      ELSE
         AX   = 0.509
         AEX  = 0.375
      ENDIF
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
c
C  DETERMINE WHICH OF THE API/ATI/RI SET OF CURVES (WINTER OR SUMMER)
C  IS NEEDED FOR THE NEXT DAYS RI CALCULATIONS.  IF SUMMER
C  CURVES ARE NEEDED, ISEA SWITCH IS SET TO 1.  IF WINTER
C  CURVES ARE NEEDED, ISEA SWITCH IS SET TO 2.  NOTE:  WINTER
C  CURVES ARE VALID FROM WEEK 38 TO WEEK 12. SUMMER CURVES ARE
C  VALID FROM WEEK 13 THROUGH WEEK 37.
c
      ISEA = 2
      IF ((IWENO .GT. 12) .AND. (IWENO .LE. 37)) ISEA = 1
c
c==================================================================
c
C  DETERMINE ATI AS A FUNCTION OF WEEK NUMBER
c==================================================================
c...  MATI(K) =   ATI VALUE FOR EVEN WEEK NUMBER
c...  TATI    =   ADJUSTED VALUE OF ATI
c
      TATI = MATI(IWENO) * .1
      SATI = TATI
c
c================================================================     
C  CHECK TO MAKE SURE WEEKLY ATI VALUE IS WITHIN VALID RANGE.
c================================================================
      IF(SATI.LT.BLIMW) THEN
        NATIBW = NATIBW + 1
        SATI = BLIMW
      ELSE IF(SATI.GT.ULIMW) THEN
        NATITW = NATITW + 1
        SATI = ULIMW
      ELSE
      ENDIF
c
c====================================================
C UPDATE RNSP ARRAY WITH RAIN/MELT FROM THIS PERIOD.
c====================================================
      IF(NSPER.GT.1) THEN
        DO 273 III=2,NSPER
273     RNSP(III-1) = RNSP(III)
      ENDIF
      RNSP(NSPER) = PXX(I)
c
c===============================================
C  CHECK TO SEE IF A STORM BREAK HAS OCCURRED.
c===============================================
      IF(NSPER.GT.1) THEN
        RNS = 0.0
        DO 277 III=1,NSPER
277     RNS = RNS + RNSP(III)
      ELSE
        RNS = RNSP(NSPER)
      ENDIF
C
C=============  STORM BREAK TEST  ===================================
      IF(PMAX.LT.RNS) THEN
c
c=====================================================================
C  IF NO STORM BREAK, THEN THE (API) IS CHANGED BUT NO CALCULATION OF
C  THE (AI) AND THE FINAL INDEX (RI) IS MADE.
C  CALCULATE RUNOFF (IF ANY) WITH THE CURRENT STORM API AND THE
C  PREVIOUS RI.
c=====================================================================
        CALL ai43 (nrel,isea,tapi,sati,ai)
        sai = ai
        tai = ai
c
c==========================================================
C       IF THERE IS A STORM BREAK, UPDATE STORM VARIABLES
C      AND CALCULATE RUNOFF (IF ANY) WITH THESE NEW VARIABLES.
c==========================================================
      ELSE
        SRO   = 0.0
        SRAIM = 0.0
        sapi  = tapi
c    ===================================================
C      CALCULATE THE (AI) BASED ON NEW STORM (API)
c    ===================================================
        CALL ai43 (nrel,isea,tapi,sati,ai)
        sai = ai
        tai = ai
c
c=====================================================================
C REVISE "AI" ACCORDING TO AVG BASIN TEMPERATURES (RI2)
C WASHBURN (IRNUM 1) IS A SPECIAL CASE.
C BUFFALO AND GENESEE BASINS USE AI FOR RI2 (NO ADJUSTMENT IN QUAD II)
c=====================================================================
        IF (IRNUM .EQ. 1) THEN
             AM = AX * ((TTBAR - 38.) ** AEX)
             RI2 = (AM * AI) + (10.0 * (1. - AM))
             RI2 = 0.625 * RI2 + 2.86
             IF (RI2 .LT. 10.0) RI2 = 10.0
        ELSEIF ((IRNUM .GE. 2) .AND. (IRNUM .LE. 96)) THEN
             AM = AX * ((TTBAR - 38.) ** AEX)
             RI2 = (AM * AI) + (10.0 * (1. - AM))
        ELSEIF (IRNUM .GE. 97) THEN
             RI2 = AI
        ELSE
        ENDIF

        IF (RI2 .GT. 80.) RI2 = 80.0
        sri = ri2
      ENDIF
C
      IF(PXX(I).GT.0.00) THEN
c
c     ==========================================================
C      SINCE THERE IS RUNOFF, UPDATE STORM RAINFALL, CALCULATE
C      TOTAL STORM RUNOFF SO FAR, AND RUNOFF FOR THIS PERIOD.
c     ==========================================================
        SRAIM=SRAIM+PXX(I)
c
c     ===========================================
C       CALCULATE RUNOFF FROM RUNOFF SUBROUTINE
c     ===========================================
        CALL RO43 (nrel,sri,sraim,cro)
c
        runoff = cro
        ROX(I) = RUNOFF - SRO
        SRO    = RUNOFF
        IF(RFCTR.GT.0.0) ROX(I) = RFCTR*ROX(I)
      ELSE
        ROX(I) = 0.0
      ENDIF
C
      RIX(I) = SRI
      APIX(I)= SAPI
      ATIX(I)= SATI
      DRO = DRO + ROX(I)
C
      IF(IFDEB.GT.0) THEN
      WRITE(IODBUG,8330) I,KDA,KHR,INTDA,INTHR,
     1  PXX(I),TAPI,TATI,TAI,SAPI,SATI,SAI,SRI,SRAIM,SRO,ROX(I),
     2  IDAY,IWENO,ISEA
8330  FORMAT(1X,I2,2X,I2,I3,I7,I5,2X,F5.2,1X,2(F5.2,2X),
     1       F5.2,1X,6(F5.2,2X),F5.2, 2(3X,I2),4X,I1)
      ENDIF
C
c=======================================
C  BEGIN FLASH FLOOD GUIDANCE
C  SAVE CARRYOVER AT LSTCMPDY FOR FFG
c=======================================
      IF(IFFG .EQ. 0) GOTO 330
      KH = I*IDELTA
      IF(INTDA .EQ. KDAFFG .AND. KH .EQ. KHRFFG) THEN
        CO(1)  = YAPI
        CO(2)  = YATI
        CO(3)  = YRI
        CO(4)  = SAPI
        CO(5)  = SATI
        CO(6)  = SRI
        CO(7)  = SRAIM
        CO(8)  = SRO
        CO(9)  = DRAIM
        CO(10) = DRO
        DO 328 J = 1, NSPER
328     CO(10+J) = RNSP(J)
        IF(IFDEB .GT. 0) THEN
          WRITE(IODBUG,8340) KMO,KDA,KYR,INTDA,KH,II,I
8340      FORMAT(/,5X,'CARRYOVER SAVED FOR FFG - DATE:',3I2,
     1           4X,'INTERNAL DAY/HOUR:',I6,'/',I2,
     2           4X,'DAY/PERIOD:',I4,'/',I2)
          WRITE(IODBUG,8342) (CO(J),J=1,10)
8342      FORMAT(5X,'YAPI    YATI    YRI     SAPI    SATI    ',
     1           'SRI    SRAIM    SRO    DRAIM    DRO',
     2           /4X,10(F5.2,3X),/)
        ENDIF
      ENDIF

C       ====  END carrover save for FLASH FLOOD GUIDANCE  =======

330   IF(I.NE.NPER) GOTO 390
c
c===================================================================
C  AT THE END OF EACH DAY, UPDATE 12Z CARRYOVER VALUES.
c===================================================================
      TAPI = CAPI
      TRI  = SRI
c
c==========================================
C  STORE PERIOD RUNOFF VALUES IN RO ARRAY.
c==========================================
      DO 350 K=1,NPER
      J = LOC + K - 1
      RO(J) = ROX(K)*25.4
350   CONTINUE
c
c======================================================
C  IF REQUESTED, STORE PERIOD API, ATI, and RI VALUES
C  IN THE APIS, ATIS, AND RIS ARRAYS, RESPECTIVELY.
c======================================================
      IF(IOFAAA.GT.0) THEN
        DO 357 K=1,NPER
        J = LOC + K - 1
        APIS(J) = APIX(K)*25.4
        RIS(J)  = RIX(K)
357     CONTINUE
c  only write out one ati per day
c  use locati which is used for pe and is 24 hr pointer
        ATIS(LOCATI) = (ATIX(1) - 32.0)/1.8
      ENDIF
c
c=============================================================
C  CHECK TO SEE IF CARRYOVER IS TO BE SAVED FOR CURRENT DATE.
c=============================================================
      IF(IFILLC.LE.0) GOTO 380
      IF(NCSTOR.LE.0) GOTO 380
      IF(NOC.GT.NCSTOR) GOTO 380
      IF(INTDA.NE.ICDAY(NOC)) GOTO 380
c
c========================================================
C IF CARRYOVER IS TO BE SAVED, STORE VALUES IN TEMPORARY
C ARRAY AND CALL SAVE CARRYOVER ROUTINE.
c========================================================
      CARY(1) = TAPI
      CARY(2) = TATI
      CARY(3) = TRI
      CARY(4) = SAPI
      CARY(5) = SATI
      CARY(6) = SRI
      CARY(7) = SRAIM
      CARY(8) = SRO
      CARY(9) = DRAIM
      CARY(10) = DRO
      DO 372 K=1,NSPER
372   CARY(10+K)=RNSP(K)
      CALL FCWTCO(INTDA,24,CARY,IUSEC)
C
      if(ifdeb.GT.0) THEN
      WRITE(IODBUG,8359) KMO,KDA,KYR,ICDAY(NOC),ICHOUR(NOC)
8359  FORMAT(/5X,'CARRYOVER SAVED - DATE:',2X,3I2,
     1       4X,'INTERNAL DAY/HOUR:',I6,'/',I2)
      WRITE(IODBUG,8360)TAPI,TATI,TRI,SAPI,SATI,SRI,SRAIM,SRO,DRAIM,DRO
8360  FORMAT(5X,'TAPI    TATI    TRI     SAPI    SATI    ',
     1'SRI    SRAIM    SRO    DRAIM    DRO',
     2/4X,10(F5.2,3X))
C      WRITE(IODBUG,8361)(RNSP(K),K=1,NSPER)
C8361  FORMAT(/,5X,'RAIM/MELT FOR EACH PERIOD WITHIN THE NEW STORM ',
C     1'WINDOW (OLDEST PERIOD IS FIRST):  ',
C     224(/12X,F5.2))
      ENDIF
c
c===============================================================
C  UPDATE COUNTER FOR NUMBER OF DAYS CARRYOVER HAS BEEN STORED.
c===============================================================
      NOC = NOC + 1
c
c==============================
C  CHECK FOR LAST DAY OF RUN
c==============================
380   IF(II.GE.LAST) GOTO 605
c
C  RESET YESTERDAYS ATI & API VALUES
      YATI = TATI
      YAPI = TAPI
C
c==================================================================
C INCREMENT TIME VALUES
c==================================================================
390   KHR = KHR + IDELTA
      IF(KHR.LE.24) GOTO 395
      KHR = KHR - 24
      KDA = KDA + 1
      IF(KDA.LE.MODA(KMO)) GOTO 395
      KDA = 1
      KMO = KMO + 1
      IF(KMO.LE.12) GOTO 394
      KMO = 1
      KYR = KYR + 1
394   IF((KMO.EQ.2).AND.(((KYR/4)*4).EQ.KYR)) MODA(2) = 29
395   INTHR = INTHR + IDELTA
      IF(INTHR.LE.24) GOTO 400
      INTHR = IDELTA
      INTDA = INTDA + 1
400   CONTINUE
C
500   CONTINUE
C==============================================================
C      END OF RUN PERIOD
C==============================================================
605   IF(IFDEB.LE.0) GOTO 610
      WRITE(IODBUG,8405)
8405  FORMAT(////5X,'TIME SERIES DATA PASSED FROM THE ',
     1      'EX43 ROUTINE BACK INTO THE CALLING ROUTINE:',
     2      //10X,'PERIOD      PX          RO        RIS',
     3      '       APIS        ATIS',/19X,'(INCHES)    (INCHES)',
     4      13X,'(INCHES)    (DEGREE)')
      DO 4409 I=1,NWD1
      X1=PX(I)/25.4
      X2=RO(I)/25.4
      IF(IOFAAA.GT.0) THEN
        X3 = APIS(I)/25.4
        J = (I-1)/4 + 1
        X4 = 1.8*ATIS(J) + 32.0
        X5 = RIS(I)
      ELSE
        X3=-999.
        X4=-999.
        X5=-999.
      ENDIF
4409  WRITE(IODBUG,8409)I,X1,X2,X5,X3,X4
8409  FORMAT(11X,I4,5X,F5.2,7X,F5.2,4X,F7.2,3X,F7.2,5X,F7.2)
c
c==============================================================
C IF LAST DATE HAS BEEN PROCESSED, CHECK TO SEE IF CARRYOVER
C VALUES SHOULD BE UPDATED IN CO ARRAY.
c==============================================================
610   IF(IFILLC.LE.0) GO TO 650
c
c================================================
C BYPASS CARRYOVER UPDATE IF FFG RUN (IFFG > 0)
c================================================
      IF(IFFG .NE. 0) GO TO 650
      CO(1) = TAPI
      CO(2) = TATI
      CO(3) = TRI
      CO(4) = SAPI
      CO(5) = SATI
      CO(6) = SRI
      CO(7) = SRAIM
      CO(8) = SRO
      CO(9) = DRAIM
      CO(10)= DRO
      DO 625 I=1,NSPER
625   CO(10+I) = RNSP(I)
C
      if(ifdeb.GT.0) THEN
      write(iodbug,8370) kmo,kda,kyr,icday(noc),ichour(noc)
8370  format(/,5x,'CARRYOVER UPDATE - DATE:',2X,3I2,
     1       4X,'INTERNAL DAY/HOUR:',I6,'/',I2)
      WRITE(IODBUG,8360)TAPI,TATI,TRI,SAPI,SATI,SRI,SRAIM,SRO,DRAIM,DRO
      ENDIF
c
c=================================================================
C  CHECK TO SEE IF ATI OR API VALUES HAVE EXCEEDED LEGAL BOUNDS.
C  IF SO, PRINT OUT WARNING MESSAGES.
c=================================================================
650   IF(NATIBW.GT.0) THEN
        WRITE(IPR,9450) BLIMW,NATIBW,BLIMW
        CALL WARN
      ENDIF
      IF(NATITW.GT.0) THEN
        WRITE(IPR,9455) ULIMW,NATITW,ULIMW
        CALL WARN
      ENDIF
      IF(NAPIT.GT.0) THEN
        WRITE(IPR,9470) NAPIT
        CALL WARN
      ENDIF
      IF(NAPIB.GT.0) THEN
        WRITE(IPR,9475) NAPIB
        CALL WARN
      ENDIF
c
c======================================================================
C  CHECK TO SEE IF REQUESTED ATI OR API MOD CORRECTIONS HAVE BEEN MADE.
c======================================================================
      IF(ICAPI.GT.NDT29) GOTO 9999
      WRITE(IPR,9510)
      CALL WARN
c
9999  IF(ITRACE.GE.1) WRITE(IODBUG,8500) SUBNAM
8500  format(' ** EXIT ',2a4)
      RETURN
C
C#######################################################################
C
C                 ERROR AND WARNING FORMAT STATEMENTS
C
C#######################################################################

9450  FORMAT(/5X,'** WARNING ** THE ATI DROPPED BELOW ',F4.1,
     1' DF (LOWER LIMIT ON THE    API/ATI/RI CURVE) ',I3,
     2' TIME(S)',/19X,'BETWEEN THE BEGINNING AND ENDING ',
     3'TIME OF THIS RUN.  THE ATI WAS RESET TO ',F4.1,
     4' EACH TIME.')
9455  FORMAT(/5X,'** WARNING ** THE ATI EXCEEDED ',F4.1,' DF ',
     1'(UPPER LIMIT ON THE    API/ATI/RI CURVE) ',I3,' TIME(S)',
     2/19X,'BETWEEN THE BEGINNING AND ENDING TIME OF THIS RUN.',
     3'  THE ATI WAS RESET TO ',F4.1,' EACH TIME.')
9470  FORMAT(/5X,'** WARNING ** THE API EXCEEDED 5.00 INCHES ',
     1I3,' TIME(S) BETWEEN THE BEGINNING AND',/19X,
     2' ENDING TIME OF THIS RUN.  THE API WAS RESET TO ',
     3'5.00 EACH TIME.')
9475  FORMAT(/5X,'** WARNING ** THE API DROPPED BELOW 0.10 INCHES ',
     1I3,' TIME(S) BETWEEN THE BEGINNING AND',/19X,
     2' ENDING TIME OF THIS RUN.  THE API WAS RESET TO ',
     3'0.10 EACH TIME.')
9510  FORMAT(/5X,'** WARNING ** THE NUMBER OF API CORRECTIONS ',
     1'REQUESTED IN THE APICQN MOD HAVE NOT BEEN PERFORMED.',
     2/19X,'CHECK TO MAKE SURE THE HOURS ENTERED INTO ',
     3'THE APICQN MOD ARE VALID.')

C#######################################################################

      END
