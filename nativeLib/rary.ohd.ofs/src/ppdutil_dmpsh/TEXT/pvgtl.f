C MEMBER PVGTL
C  (from old member PDBDMPSH)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/19/95.10:00:36 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  PGM: PVGTL(UWC,UWT,LSTYP,LSMTF,NOFTT,NOFT,ICOND) .. GET DATA TYP LIST
C
C
C   IN: UWC ........ UNIT NUMBER TO OUTPUT PROMPTS TO USER - INT
C   IN: UWT ........ UNIT NUMBER FOR OUTPUT TO TERMINAL DEVICE - INT
C  OUT: LSTYP(1) ... LIST OF ALLOWABLE TYP (4 CHR/TYP), USES 'ALL' - INT
C  OUT: LSMTF(1) ... INDICATORS TO INCLUDE MISSG DATA (0,2=NO  1,3=YES),
C  OUT:              OR ESTIMATED DATA (0,1=NO  2,3=YES) - INT
C   IN: NOFTT ...... TOTAL NUM OF DATA TYPES POSSIBLE, (LSTYP DIM) - INT
C  OUT: NOFT ....... NUMBER OF DATA TYPES IN LSTYP - INT
C  I/O: ICOND ...... PGM COND: IF NOT 0 SKIP, SET 1 = ERR, NO TYP - INT
C   IN: ICD ........ (FROM COMMON) UNIT NUM FOR INPUT (IF USED) - INT
C   IN:                NOTE, ICD IS USED IN ROUTINE RPCARD
C
C
C  RQD: SUBPROGRAMS:  RPCARD,UFREE,UMEMST,UPACK1,UNAMCP,UMEMOV
C  RQD: SUBPROGRAMS:  PVSUBB,PVSUBE
C  RQD: COMMON:       UIO,UFREEI
C
C
C  HIS: WRITTEN BY D. STREET IN MAY 1988
C  =====================================================================
      SUBROUTINE PVGTL (UWC,UWT,LSTYP,LSMTF,NOFTT,NOFT,ICOND)
C
C
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
C
C
C
      INTEGER    UWC,UWT,NOFTT,NOFT,ICOND
      INTEGER    IDOLR(2)
      INTEGER    ICMD(2),ITYP(2),IBLNK,IAMP(2),NU,IB,IE,N,I,IERR
      INTEGER    LSTYP(64),LSMTF(64),ISLM(2),MISSG,IDAY(2),IDLY,ROUTN(2)
      INTEGER    ISLE(2),ESTMG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvgtl.f,v $
     . $',                                                             '
     .$Id: pvgtl.f,v 1.2 1997/06/25 13:23:02 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA IDOLR/4H$   ,4H    /
      DATA       ITYP,IBLNK,IAMP / 4HDTYP,4HE   ,4H    ,4H&   ,4H     /
      DATA       IDAY,IDLY,ROUTN / 4HDAIL,4HY   ,4HDLY ,4HPVGT,4HL    /
      DATA       ISLM,ISLE       / 4H/M  ,4H    ,4H/E  ,4H            /
C
C
C
C                  CALL TRACE ROUTINE FOR ENTRY INTO THIS ROUTINE
C
      CALL PVSUBB (ROUTN,ICOND)
C
C                  SKIP THIS RTN IF ICOND INDICATES PREVIOUS GLITCH
C
      IF ( ICOND.NE.0 ) GO TO 130
C
C                  INITIALIZE NUMBR OF DATA TYPES FOUND IN INPUT LINE (S
C
      NOFT = 0
C
C                  OUTPUT PROMPT IF UNIT NUM IS SAME AT TERMINL UNIT NUM
C
      IF ( UWC.EQ.UWT ) WRITE (UWC,10)
10    FORMAT(' NOW ENTER A LIST OF DATA TYPES (CONTINUE LINES WITH &):')
C
C                  INPUT LINE OF DATA TYPES INTO 72A1 FREE FORMAT BUFFER
C
15    CALL RPCARD (IBUF,IERR)
      CALL WPCARD (IBUF)
        IF ( IERR.NE.0 ) GO TO 110
C
C                  ORGANIZE WORDS INTO FIELDS USING 'UFREEI' VARIABLES
C
      CALL UFREE (1,72)
      N = 0
      IF ( NFIELD.LE.0 ) GO TO 110
C
      IB = IFSTRT(1)
      IE = IFSTOP(1)
      NU = IE-IB+1
      IF ( NU.GT.8 ) NU = 8
      CALL UMEMST (IBLNK,ICMD,2)
      CALL UPACK1 (IBUF(IB),ICMD,NU)
C
C                  CHECK IF FIRST FIELD IS '$', IF SO THEN SKIP IT
C
      CALL UNAMCP (ICMD,IDOLR,I)
      IF ( I.EQ.0 ) GO TO 15
C
C                  CHECK IF FIRST FIELD IS 'DTYPE', IF SO THEN SKIP IT
C      
      CALL UNAMCP (ICMD,ITYP,I)
      IF ( I.EQ.0 ) N = N+1
      IF ( N.LT.NFIELD ) GO TO 20
C
C                  IF ONLY 'DTYPE' IS ON LINE, INPUT ANOTHER LINE
C
        CALL RPCARD (IBUF,IERR)
        CALL WPCARD (IBUF)
          IF ( IERR.NE.0 ) GO TO 110
        CALL UFREE (1,72)
        N = 0
C
20    CONTINUE
C
C      - - - - - - START LOOP FOR EACH DATA TYPE FIELD
C
30    IF ( N.GE.NFIELD ) GO TO 130
C
        N = N+1
        IB = IFSTRT(N)
        IE = IFSTOP(N)
        NU = IE-IB+1
C
C                  PUT DATA TYP IN ICMD, CHECK FOR '/M' AS LAST TWO CHRS
C                  (IF '/M' FOUND, SET MISSG-DATA-TO-BE-OUTPUT FLAG = 1)
C                  ALSO CHCK FOR '/E' FOR ESTIMATED DATA (ADD 2 TO FLAG)
C
          MISSG = 0
          ESTMG = 0
40        IF ( NU.LE.2 ) GO TO 60
          CALL UMEMST (IBLNK,ICMD,2)
          CALL UPACK1 (IBUF(IE-1),ICMD,2)
          CALL UNAMCP (ICMD,ISLM,I)
          IF ( I.NE.0 ) GO TO 50
            MISSG = 1
            IE = IE-2
            NU = NU-2
              GO TO 40
50        CALL UNAMCP (ICMD,ISLE,I)
          IF ( I.NE.0 ) GO TO 60
            ESTMG = 2
            IE = IE-2
            NU = NU-2
              GO TO 40
60        CONTINUE
C
C                  CHECK TO SEE IF FIELD IS '&' FOR CONTINUATION OF LINE
C
        IF ( NU.GT.8 ) NU = 8
        CALL UMEMST (IBLNK,ICMD,2)
        CALL UPACK1 (IBUF(IB),ICMD,NU)
        CALL UNAMCP (ICMD,IAMP,I)
        IF ( I.EQ.0 ) GO TO 70
C
C                  CHANGE 'DAILY' TO 'DLY'
C
        CALL UNAMCP (ICMD,IDAY,I)
        IF ( I.EQ.0 ) ICMD(1) = IDLY
C
C                  IF NOT '&', THEN PUT TYPES INTO STATION LIST, LSTYP,
C                              INCREMENT NO-OF-STATNS, NOFT,
C                              PUT MIS-EST DATA INDICATOR IN LIST, LSMTF
C
          IF ( NOFT.GE.NOFTT ) GO TO 90
          NOFT = NOFT+1
          CALL UMEMOV (ICMD,LSTYP(NOFT),1)
          LSMTF(NOFT) = MISSG+ESTMG
            GO TO 80
C
C                  ELSE IF '&', THEN INPUT A NEW LINE OF DATA TYPES
C
70        CALL RPCARD (IBUF,IERR)
          CALL WPCARD (IBUF)
            IF ( IERR.NE.0 ) GO TO 110
          CALL UFREE (1,72)
          N = 0
C
80      CONTINUE
          GO TO 30
C
C      - - - - - - END LOOP FOR EACH DATA TYPE FIELD
C
C                  WARNING FOR ARRAY SIZE TOO SMALL AND EXIT FROM LOOP
C
90    WRITE (LPE,100) ICMD
100   FORMAT(' **WARNING** TOO MANY STATNS FOR LSTYP ARRAY STARTING',
     *      /'             WITH INPUT STATION, ',2A4)
        GO TO 130
C
C                  ERROR MESSAGE AND ABORT ROUTINE
C
110   ICOND = 1
      WRITE (LPE,120)
120   FORMAT(' **ERROR** IN PVGTL, BAD READ FROM RPCARD')
C
C                  CALL TRACE ROUTINE FOR EXIT FROM THIS ROUTINE
C
130   CONTINUE
      CALL PVSUBE (ROUTN,ICOND)
C
C
      RETURN
C
      END
