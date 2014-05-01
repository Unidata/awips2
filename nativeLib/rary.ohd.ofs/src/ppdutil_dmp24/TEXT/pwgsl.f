C MODULE PWGSL
C-----------------------------------------------------------------------
C
      SUBROUTINE PWGSL (UWC,UWT,LSSTA,LSMSF,NOFST,NOFS,KNTL,KPP,KZK,KZ,
     *   ICOND)
C
C   IN: UWC ........ UNIT NUMBER TO OUTPUT PROMPTS TO USER - INT
C   IN: UWT ........ UNIT NUMBER FOR OUTPUT TO TERMINAL DEVICE - INT
C  OUT: LSSTA(1) ... LIST OF ALLOWABLE STA (8 CHR/STA), USES 'ALL' - INT
C  OUT: LSMSF(1) ... INDICATORS TO INCLUDE CERTAIN DATA TYPES - INT
C   IN: NOFST ...... TOTAL NUM OF STA POSSIBLE, (DIM OF LSSTA)/2 - INT
C  OUT: NOFS ....... NUMBER OF STATION NAMES IN LSSTA - INT
C  OUT: KNTL ....... CONTROL VARIABLE FOR SORTING OPTIONS - INT
C  OUT:            KNTL = 0 ... NO SORTING
C  OUT:            KNTL = 1 ... BY STATE, THEN STATION-ID
C  OUT:            KNTL = 2 ... BY STATE, THEN STA-DESCRIPTION
C  OUT:            KNTL = 3 ... BY STATE, THEN BY PCPN, THEN BY STA-ID
C  OUT:            KNTL = 4 ... BY STATE, THEN BY PCPN, THEN BY STA-DESC
C  OUT:            KNTL = 5 ... BY STATION-ID ONLY
C  OUT:            KNTL = 6 ... BY STA-DESCRIPTION ONLY
C  OUT:            KNTL = 7 ... BY PCPN, THEN BY STA-ID
C  OUT:            KNTL = 8 ... BY PCPN, THEN BY STA-DESCRIPTION
C  OUT: KPP ........ MINIMUM PCPN LEVEL ACCEPTIBLE (100 INCH) - INT
C  OUT: KZK ........ IF 1 THEN DISPLAY HR OFFSET, IF 0 NO DISPLAY - INT
C  OUT: KZ ......... TIME ZONE CODE - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1 = ERR, NO STA - INT
C   IN: ICD ........ (FROM COMMON) UNIT NUM FOR INPUT (IF USED) - INT
C   IN:                NOTE, ICD IS USED IN ROUTINE RWCARD
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hdflts'
C
      INTEGER    UWC,UWT,NOFST,NOFS,ICOND,KNTL,KPP,KZK,KZ
      INTEGER    ICMD(5),ISTA(2),IAMP(2)
      INTEGER    LSSTA(200),LSMSF(100),ROUTN(2)
      CHARACTER*8  IDOLR
      INTEGER    IEND(2),ISTY(2),IOBS(2),IDDD(2),IDES(2),IALL(2)
      INTEGER    ISTN(2),IQQQ(2),IQUA(2),LALL(2),IHR1(2),IHR2(2)
      INTEGER    ICMDX(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmp24/RCS/pwgsl.f,v $
     . $',                                                             '
     .$Id: pwgsl.f,v 1.4 2000/09/27 16:22:11 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA       IDOLR / '$       ' /
      DATA       KHRQ,KHRLP,KHRRP / 1HQ,1H(,1H) /
      DATA       ISTA,IBLNK,IAMP / 4HSTAI,4HD   ,4H    ,4H&   ,4H     /
      DATA            ROUTN      /               4HPVGS,4HL           /
      DATA       ICHMM,ICHPP,ICHEE,ICHFF / 4HM   ,4HP   ,4HE   ,4HF    /
      DATA       ISLSH / 4H/    /
      DATA       IEND,ISTY  /  4HEND ,4H    ,  4HSTAT,4HE     /
      DATA       IOBS,IDDD  /  4HOBSO,4HNLY ,  4HID  ,4H      /
      DATA       IDES,IALL  /  4HDESC,4H    ,  4HALL ,4H      /
      DATA       ISTN,IQQQ  /  4HNOST,4HATE ,  4HQ   ,4H     /
      DATA       IQUA,LALL  /  4HQUAN,4HTITY,  4HALL ,4H     /
      DATA       IHR1,IHR2  /  4HHR  ,4H    ,  4HHR( ,4H     /
C
C
      CALL PVSUBB(ROUTN,ICOND)
C
10    NOFS = 0
C
C  OUTPUT PROMPT IF UNIT NUM IS SAME AT TERMINL UNIT NUM
      IF ( UWC.EQ.UWT ) WRITE (UWC,20)
20    FORMAT (1H0)
C 100 FORMAT (//' ENTER THE FOLLOWING SERIES (END STMT MUST BE LAST):'
C    */        '    STATE  . . . . LIST ALPHABTCL BY STATE-DESCRPTN, OR'
C    */        '    STATE DESC . . LIST ALPHABTCL BY STATE-DESCRPTN, OR'
C    */        '    STATE ID . . . LIST ALPHABTCL BY STATE-IDENTFR, OR'
C    */        '    STATE QUANTITY LIST ALPHABTCL BY STATE-DCN PCPN, OR'
C    */        '    QUANTITY . . . LIST BY DECENDING PCPN-IDENTIFR, OR'
C    */        '    DESC . . . . . LIST BY DESCRIPTION ONLY, OR'
C    */        '    ID . . . . . . LIST BY STATION IDENDIFIER ONLY, OR'
C    */        '    QUANTITY DESC  LIST BY DECNDG PCPN-DESCRPTN,'
C    */        '                   ELSE LIST BY DECENDING PCPN AMOUNTS,'
C    */        '   PLUS:'
C    */        '    OBSONLY  . . . DO NOT LIST ESTIMATED AMOUNTS, OR'
C    */        '    ALL  . . . . . LIST FULL, PARTIAL, AND EST AMOUNTS,'
C    */        '                   ELSE LIST FULL, PARTL, EST AMOUNTS,'
C    */        '   PLUS OPTIONAL INTGR OR REAL INCHES FOR MIN PCPN LIM,'
C    */        '   PLUS:'
C    */        '    HR   . . . . . TO APPEND HOUR OFFSET TO DATA, OR'
C    */        '    HR(TTT)  . . . APPEND HR OFFSET, TIME ZONE TTT, OR'
C    */        '    (TTT)  . . . . TO USE ANOTHER TIME ZONE'
C    */        '   PLUS:'
C    */        '    END  . . . . . END CONTROL INPUT (MUST BE LAST)'
C    *//       ' ELSE ENTER THE FOLLOWING COMMAND FOR SPECIAL INPUT:'
C    *//       '    STAID  . . . . FOR STATION-LIST SORT-CNTRL PROMPT'
C    */)
C
C  READ CARD
      CALL RWCARD(IERR)
      IF ( IERR.NE.0 ) GO TO 410
C
C  GET FIELDS
      CALL UFREE(1,72)
        IF ( NFIELD.LE.0 ) GO TO 410
C
C  CHECK IF FIRST FIELD IS 'STAID', IF SO THEN SKIP IT
      IB = IFSTRT(1)
      IE = IFSTOP(1)
      NU = IE-IB+1
      IF ( NU.GT.8 ) NU = 8
      CALL UMEMST (IBLNK,ICMD,5)
      CALL UPACK1 (IBUF(IB),ICMD,NU)
      CALL UNAMCP (ICMD,ISTA,I)
C
C  IF INPUT DOES NOT START WITH 'STAID' GO TO ORDINARY
C  BATCH TYPE INPUT WITH LIMITED CONTROLS
      IF ( I.NE.0 ) GO TO 190
C
C  CHECK IF STATIONS ARE ON 'STAID' LINE, ELSE DO PROMPT
      N = 1
      IF ( NFIELD.GT.1 ) GO TO 40
C
C  OK, THEN SPECIALIZED INPUT BY STATION-ID IS USED
      IF ( UWC.EQ.UWT ) WRITE (UWC,30)
30    FORMAT (//' ENTER LIST OF STATIONS, APPEND THE FOLLOWING OPTIONS'
     */        '  ELSE A DEFAULT OF /F/P/E WILL BE USED (+ IS WILD-CHAR'
     */        '  IN STATION ID SUCH AS +++C2 FOR ALL COLORADO STATNS):'
     *//       '         /F ... FULL DATA'
     */        '         /M ... MISSING DATA'
     */        '         /P ... PARTIAL DATA'
     */        '         /E ... ESTIMATED DATA'
     *//)
C
C  IF ONLY 'STAID' IS ON LINE, INPUT ANOTHER LINE
      CALL RWCARD (IERR)
      IF ( IERR.NE.0 ) GO TO 410
      CALL UFREE (1,72)
      N = 0
C
40    CONTINUE
C
C   LOOP FOR EACH STATION ID FIELD
C
50    IF ( N.GE.NFIELD ) GO TO 110
C
        N = N+1
        IB = IFSTRT(N)
        IE = IFSTOP(N)
        NU = IE-IB+1
C
C  CHECK TO SEE IF FIELD IS '&' FOR CONTINUATION OF LINE
        IF ( NU.GT.20 ) NU = 20
        CALL UMEMST (IBLNK,ICMD,5)
        CALL UPACK1 (IBUF(IB),ICMD,NU)
        IF ( ICMD(1).NE.IAMP(1) ) GO TO 60
C
C  IF '&', THEN INPUT A NEW LINE OF STATIONS
          CALL RWCARD(IERR)
            IF ( IERR.NE.0 ) GO TO 410
          CALL UFREE(1,72)
          N = 0
            GO TO 50
C
C
C                  IF NOT '&', THEN PUT STA ID INTO STATION LIST, LSSTA,
C                              INCREMENT NO-OF-STATNS, NOFS,
C                              PUT DATA INDICATOR IN LIST, LSMSF
C
C                  DECODE STATION OPTIONS APPENDED TO GIVEN STATION WITH
C                  A SLASH.  OPTIONS ARE...
C                      /F  ...  INCLUDE STATIONS WITH FULL DATA (DFLT)
C                      /M  ...  INCLUDE STATIONS WITH ALL MISSG DATA
C                      /P  ...  INCLUDE STATIONS WITH PARTIAL DATA (DFT)
C                      /E  ...  INCLUDE STATIONS WITH ESTIMATED DATA (D)
C
C                  OUTPUT CONTROL VARIABLE, KOPTN, HAS THESE FLAGS ...
C                      01  ...  INCLUDE FULL DATA STATIONS (DEFAULT)
C                      02  ...  INCLUDE ESTIMATED VALUE STATIONS (DFLT)
C                      04  ...  INCLUDE PARTIAL DATA STATIONS (DEFAULT)
C                      08  ...  INCLUDE MISSING DATA STATIONS
C
C
60      KOPF = 0
        KOPE = 0
        KOPP = 0
        KOPM = 0
70      IF ( NU.LE.2 ) GO TO 100
        IF ( IBUF(IE-1).NE.ISLSH ) GO TO 100
          ICHAR = IBUF(IE)
          IF ( ICHAR.EQ.ICHFF ) KOPF = 1
          IF ( ICHAR.EQ.ICHEE ) KOPE = 1
          IF ( ICHAR.EQ.ICHPP ) KOPP = 1
          IF ( ICHAR.EQ.ICHMM ) KOPM = 1
          IF ( KOPM+KOPP+KOPE+KOPF.NE.0 ) GO TO 90
            WRITE (LP,80) ICHAR
80          FORMAT (' **WARNING** BAD OPTION CHAR AFTER STATION - ',A4)
90        CONTINUE
          IE = IE-2
          NU = NU-2
            GO TO 70
100     CONTINUE
        KOPTN = 8*KOPM+4*KOPP+2*KOPE+KOPF
C
C    DEFAULT VALUE OF 7 FOR /F/P/E
        IF ( KOPTN.EQ.0 ) KOPTN = 7
C
          IF ( NOFS.GE.NOFST ) GO TO 170
          M = NOFS+NOFS+1
          NOFS = NOFS+1
          CALL UMEMST (IBLNK,LSSTA(M),2)
          CALL UPACK1 (IBUF(IB),LSSTA(M),NU)
          LSMSF(NOFS) = KOPTN
C
            GO TO 50
C
110   CONTINUE
C
C          KNTL CONTROLS SORTING RULES FOR OUTPUT:
C            KNTL = 0 ... NO SORTING
C            KNTL = 1 ... SORT BY STATE, THEN STATION-ID
C            KNTL = 2 ... SORT BY STATE, THEN STA-DESCRIPTION
C            KNTL = 3 ... SORT BY STATE, THEN BY PCPN, THEN BY STA-ID
C            KNTL = 4 ... SORT BY STATE, THEN BY PCPN, THEN BY STA-DESC
C            KNTL = 5 ... SORT BY STATION-ID ONLY
C            KNTL = 6 ... SORT BY STA-DESCRIPTION ONLY
C            KNTL = 7 ... SORT BY PCPN, THEN BY STA-ID
C            KNTL = 8 ... SORT BY PCPN, THEN BY STA-DESCRIPTION
C
C
      IF ( UWC.EQ.UWT ) WRITE (UWC,120)
120   FORMAT (//' ENTER SORT CONTROL NUMBER:'
     *//       '        1  . . . . BY STATE, THEN STA-ID'
     */        '        2  . . . . BY STATE, THEN STA-DESC'
     */        '        3  . . . . BY STATE, THEN PCPN, THEN ID'
     */        '        4  . . . . BY STATE, THEN PCPN, THEN DESC'
     */        '        5  . . . . BY STATION-ID ONLY'
     */        '        6  . . . . BY STATION-DESC ONLY'
     */        '        7  . . . . BY PCPN, THEN STA-ID'
     */        '        8  . . . . BY PCPN, THEN STA-DESC'
     *//)
130   READ(ICD,*) KNTL
      IF ( UWC.NE.UWT ) GO TO 150
      IF ( KNTL.GT.0.AND.KNTL.LT.9 ) GO TO 150
        WRITE (UWC,140)
140     FORMAT (//' WRONG, RE-ENTER A CORRECT NUMBER:')
          GO TO 130
150     CONTINUE
C
C  GET THE MINIMUM ALLOWABLE PCPN AMOUNT
      IF ( UWC.EQ.UWT ) WRITE (UWC,160)
160   FORMAT (/' ENTER MIN ACCEPTABLE PCPN AS INTGR (100THS INCH):'/)
      READ(ICD,*) KPP
      KZK = 0
      KZ  = TIME(3)
      GO TO 430
C
170   WRITE (LP,180) ICMD(1),ICMD(2)
180   FORMAT ('0**WARNING** TOO MANY STATNS FOR PROGRAM ARRAY STARTING',
     *      /'             WITH INPUT STATION, ',2A4)
        GO TO 430
C
C   BATCH TYPE INPUT
190     KPP  = 0
        KZK  = 0
        KZ   = TIME(3)
        KNTL = 7
        NOFS = 1
        M    = NOFS+NOFS-1
        LSSTA(M)   = LALL(1)
        LSSTA(M+1) = LALL(2)
        LSMSF(NOFS) = 7
C
C    SET UP DEFAULTS FOR KEYWORD INPUT
C    (JIDEN = 1 FOR SORT BY ID FOR SAME PCPN VALUE)
        JSTAT = 0
        JIDEN = 1
        JQUAN = 1
C
C  DECODE FIRST INPUT WORD
        N = 0
200     IF ( N.LT.NFIELD ) GO TO 210
C
C  INPUT NEXT LINE
            CALL RWCARD(IERR)
            IF ( IERR.NE.0 ) GO TO 410
            CALL UFREE(1,72)
            N = 0
            GO TO 200
210     N  = N+1
        IB = IFSTRT(N)
        IE = IFSTOP(N)
        NU = IE-IB+1
        IF ( NU.GT.8 ) NU = 8
        CALL UMEMST (IBLNK,ICMD,5)
        CALL UPACK1 (IBUF(IB),ICMD,NU)
C
C  CHECK FOR KEYWORDS
C    '$' (COMMENT)
        CALL UNAMCP (ICMD,IDOLR,I)
        IF ( I.EQ.0 ) GO TO 200
C    'END'
        CALL UNAMCP (ICMD,IEND,I)
        IF ( I.EQ.0 ) GO TO 380
C    'STATE'
        CALL UNAMCP (ICMD,ISTY,I)
        IF ( I.NE.0 ) GO TO 220
          JSTAT = 2
          IF ( JQUAN.EQ.1 ) JQUAN = 0
          IF ( JIDEN.EQ.1 ) JIDEN = 0
            GO TO 200
C    'NOSTATE'
220     CALL UNAMCP (ICMD,ISTN,I)
        IF ( I.NE.0 ) GO TO 230
          JSTAT = 0
          IF ( JQUAN.EQ.0 ) JQUAN = 1
            GO TO 200
C    'ID'
230     CALL UNAMCP (ICMD,IDDD,I)
        IF ( I.NE.0 ) GO TO 240
          JIDEN = 2
          IF ( JQUAN.EQ.1 ) JQUAN = 0
            GO TO 200
C    'DESC'
240     CALL UNAMCP (ICMD,IDES,I)
        IF ( I.NE.0 ) GO TO 250
          JIDEN = 0
          IF ( JQUAN.EQ.1 ) JQUAN = 0
            GO TO 200
C    'Q' OR 'QUANTITY'
250     CALL UNAMCP (ICMD,IQQQ,I)
        IF ( I.NE.0 ) CALL UNAMCP (ICMD,IQUA,I)
        IF ( I.NE.0 ) GO TO 260
          JQUAN = 2
        IF ( IB+NU.GT.IE ) GO TO 200
          IB = IB+NU
            GO TO 320
C   'OBSONLY'
260     CALL UNAMCP (ICMD,IOBS,I)
        IF ( I.NE.0 ) GO TO 270
          LSMSF(NOFS) = 5
            GO TO 200
C    'ALL'
270     CALL UNAMCP (ICMD,IALL,I)
        IF ( I.NE.0 ) GO TO 280
          LSMSF(NOFS) = 7
            GO TO 200
C    'HR'
280     CALL UNAMCP (ICMD,IHR1,I)
        IF ( I.NE.0 ) GO TO 290
          KZK = 1
            GO TO 200
C    'HR('
290     CALL UMEMST (IBLNK,ICMDX,2)
        CALL UMOVEX (ICMD,1,ICMDX,1,3)
        CALL UNAMCP (ICMDX,IHR2,I)
        IF ( I.NE.0 ) GO TO 300
          KZK = 1
          I = IE-IB-3
          IF ( I.LT.1 ) GO TO 200
          CALL UMEMST (IBLNK,ICMDX,1)
          CALL UMOVEX (ICMD,4,ICMDX,1,I)
          IZON = ICMDX(1)
          CALL FCITZC(IZT,IDSAV,IZON)
          IF ( IZT.NE.100 ) KZ = IZON
            GO TO 200
C     CHECK FOR VALID TIME ZONE
300     IZON = ICMD(1)
        CALL FCITZC(IZT,IDSAV,IZON)
        IF ( IZT.EQ.100 ) GO TO 310
          KZ = IZON
            GO TO 200
C                                     '&'
310     CALL UNAMCP (ICMD,IAMP,I)
        IF ( I.NE.0 ) GO TO 320
            GO TO 200
C    FIND A NUMBER
320     CONTINUE
        IBT = IB
        IET = IE
        IF ( IBUF(IBT ).EQ.KHRQ ) IBT = IBT+1
        IF ( IBUF(IBT).NE.KHRLP ) GO TO 330
        IF ( IBUF(IET).NE.KHRRP ) GO TO 330
          IBT = IBT+1
          IET = IET-1
          IF ( IBT.GT.IET ) GO TO 360
330     CONTINUE
C                                       (CHECK FOR INTEGER)
        CALL UCKINT(IBUF,IBT,IET,I)
        IF ( I.NE.0 ) GO TO 340
          CALL UNUMIC(IBUF,IBT,IET,KPP)
          KPP = 100*KPP
            GO TO 350
340     CONTINUE
C                                       (CHECK FOR REAL, IF SO, X100)
        CALL UCKFLT(IBUF,IBT,IET,KK,I)
        IF ( I.NE.0 ) GO TO 360
          NN  = 0
          IF ( KK.GT.IBT ) CALL UNUMIC(IBUF,IBT,KK-1,NN)
          KPP = 100*NN
          NN  = 0
          KK  = KK+1
          IF ( KK.LE.IET ) CALL UNUMIC(IBUF,KK,KK,NN)
          KPP = KPP+10*NN
          NN  = 0
          KK  = KK+1
          IF ( KK.LE.IET ) CALL UNUMIC(IBUF,KK,KK,NN)
          KPP = KPP+NN
          NN  = 0
          KK  = KK+1
          IF ( KK.LE.IET ) CALL UNUMIC(IBUF,KK,IET,NN)
          IF ( NN.GT.0 ) KPP = KPP+1
350     CONTINUE
        IF ( IBUF(IB).EQ.KHRQ ) JQUAN = 2
            GO TO 200
360     CONTINUE
C
C  CANNOT IDENTIFY KEYWORD
          WRITE (LP,370) ICMD(1),ICMD(2)
370       FORMAT (' **WARNING** BAD COMMAND KEYWORD - ',2A4,
     *           ' - THE COMMAND IS IGNORED')
            GO TO 10
C
380     CONTINUE
        IF ( JSTAT.EQ.0 ) GO TO 390
          IF ( JIDEN.EQ.0.AND.JQUAN.EQ.0 ) KNTL = 2
          IF ( JIDEN.NE.0.AND.JQUAN.EQ.0 ) KNTL = 1
          IF ( JIDEN.EQ.0.AND.JQUAN.NE.0 ) KNTL = 4
          IF ( JIDEN.NE.0.AND.JQUAN.NE.0 ) KNTL = 3
            GO TO 400
390       IF ( JIDEN.EQ.0.AND.JQUAN.EQ.0 ) KNTL = 6
          IF ( JIDEN.NE.0.AND.JQUAN.EQ.0 ) KNTL = 5
          IF ( JIDEN.EQ.0.AND.JQUAN.NE.0 ) KNTL = 8
          IF ( JIDEN.NE.0.AND.JQUAN.NE.0 ) KNTL = 7
400     CONTINUE
          GO TO 430
C
410   ICOND = 1
      WRITE (LP,420)
420   FORMAT ('0**ERROR** IN PWGSL, BAD READ FROM RWCARD')
C
430   CALL PVSUBE(ROUTN,ICOND)
C
      RETURN
C
      END
