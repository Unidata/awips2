C MODULE FCIDCK
C-----------------------------------------------------------------------
C
C  THIS ROUTINE CHECKS FOR A VALID IDENTIFIER.
C
      SUBROUTINE FCIDCK (ID,NCHAR,IPACKD,IDTYPE,IPRINT,ISTAT)
C
C  IDENTIFIERS ARE CHECKED FOR:
C     - RESERVED WORD OR ALL BLANKS
C     - INVALID CHARACTER ANYWHERE IN IDENTIFIER
C     - INVALID FIRST CHARACTER OR BLANK FIRST CHARACTER
C     - TOO MANY CHARACTERS
C
C  ARGUMENT LIST:
C     ID     - IDENTIFIER TO BE CHECKED
C     NCHAR  - NUMBER OF CHARACTERS IN ID
C     IPACKD - INDICATOR WHETHER OR NOT IDENTIFIER IS PACKED
C               0=NOT PACKED (1 CHARACTER IN LEFT MOST BYTE OF WORD)
C               1=PACKED (4 CHARACTERS PER WORD)
C     IDTYPE - CODE FOR IDENTIFIER TYPE
C               1=HCL IDENTIFIER (PROCEDURE, FUNCTION OR TECHNIQUE NAME)
C               2=PREPROCESSOR COMPONENT (AREA IDENTIFIER)
C               4=TIME SERIES IDENTIFIER
C               5=OPERATION NAME
C     IPRINT - OPTION TO CONTROL PRINTING OF ERROR MESSAGES
C               0=DO NOT PRINT MESSAGES
C               1=PRINT ERROR MESSAGES
C     ISTAT  - STATUS CODE:
C               0=NO ERRORS, IDENTIFIER IS VALID
C               1=ID IS ONE OF THE RESERVED WORDS FOR THE IDENTIFIER 
C                 TYPE OR IDENTIFIER IS ALL BLANKS
C               2=ID CONTAINS A CHARACTER NOT ALLOWED FOR THE IDENTIFIER
C                 TYPE, OR FIRST CHARACTER OF IDENTIFIER IS BLANK
C               3=TOO MANY CHARACTERS IN ID, FIRST N CHARACTERS ARE
C                 VALID
C               4=TOO MANY CHARACTERS IN ID, FIRST N CHARACTERS ARE A
C                 RESERVED WORD FOR THE IDENTIFIER TYPE
C               5=TOO MANY CHARACTERS IN ID, FIRST N CHARACTERS CONTAIN
C                 A CHARACTER NOT ALLOWED FOR THE IDENTIFIER TYPE
C               6=NCHAR, IPACKD OR IDTYPE IS NOT IN VALID RANGE,
C                 A MESSAGE IS PRINTED AND ERROR CALLED UNLESS IPRINT IS
C                 ZERO
C
C   ONE LIST OF RESERVED WORDS AND ONE LIST OF INVALID CHARACTERS IS
C   KEPT IN THIS ROUTINE.  THE IDENTIFIER TYPES TO WHICH THE WORDS
C   AND CHARACTERS APPLY IS DECIDED AS FOLLOWS:
C    - EACH IDENTIFIER TYPE IS ASSIGNED A DIFFERENT PRIME NUMBER.
C    - WHEN A RESERVED WORD IS ENTERED IN ARRAY RSVWRD THE 4TH VALUE
C      FOR THAT WORD IS AN INTEGER WHICH IS THE PRODUCT OF THE PRIME
C      NUMBERS FOR THE IDENTIFIER TYPES FOR WHICH THAT WORD APPLIES.
C    - AS AN IDENTIFIER IS CHECKED AGAINST THE LIST OF RESERVED WORDS, 
C      IT IS CHECKED AGAINST ONLY THOSE WORDS FOR WHICH ITS PRIME NUMBER
C      DIVIDES EVENLY.
C    - WHEN AN INVALID CHARACTER IS ENTERED IN ARRAY INVLDC THE 2ND
C      VALUE FOR THAT WORD IS AN INTEGER WHICH IS THE PRODUCT AS FOR
C      RESERVED WORDS.
C
C  VALUES FOR CHECKING IDENTIFIERS:  
C     ID TYPES     PRIME NUMBER     IDTYPE
C     --------     ------------     ------
C     SYSTEM             3             1
C     PP                 5             2
C     FC                 7             3
C     TS                11             4
C     OPNAME            13             5
C   NOTE THE FOLLOWING PRODUCTS OF PRIME NUMBERS:
C       1155 = 3 * 5 * 7 * 11
C      15015 = 3 * 5 * 7 * 11 * 13
C
C
      CHARACTER*1 CHAR1
      CHARACTER*7 TYPMSG
      CHARACTER*8 OLDOPN
C      
      LOGICAL*1 INVCA1(4)
      LOGICAL*4 RESVRD,INVCHR,INFCHR,TMCHAR
C
      INTEGER*4 RSVWRD,PACKWD,UNPKWD
C
      CHARACTER*30 CDTYPE(5)
     *   /'HCL IDENTIFIER',
     *    'PREPROCESSOR IDENTIFIER',
     *    'FORECAST COMPONENT IDENTIFIER',
     *    'TIME SERIES IDENTIFIER',
     *    'OPERATION NAME'/
C
      INTEGER MXCHAR(10)/8,8,8,8,8,5*0/
      INTEGER IVAL(10)/3,5,7,11,13,17,19,23,29,31/
C
      DIMENSION ID(*)
      DIMENSION RSVWRD(4,20),INVLDC(2,10),INVFCH(2,5)
      DIMENSION PACKWD(3),UNPKWD(12)
C
      EQUIVALENCE (INVCA4,INVCA1(1))
C      
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/fcidck.f,v $
     . $',                                                             '
     .$Id: fcidck.f,v 1.4 2000/03/13 20:52:31 page Exp $
     . $' /
C    ===================================================================
C
C
      DATA RSVWRD/4HOBSO,4HLETE,4H    ,7,    4HALL ,4H    ,4H    ,3,
     *            4HINCL,4H    ,4H    ,3,    4HINCL,4HUDE ,4H    ,3,
     *            4HMOD ,4H    ,4H    ,3,    4HEND ,4H    ,4H    ,3,
     *            4HS   ,4H    ,4H    ,1155, 4HF   ,4H    ,4H    ,1155,
     *            4HC   ,4H    ,4H    ,1155, 4HA   ,4H    ,4H    ,1155,
     *            4HINPU,4HT CO,4H    ,13,   4HENDM,4HOD  ,4H    ,3,
     *            4H&   ,4H    ,4H    ,15015,4HMOD ,4H    ,4H    ,3,
     *            4HENDP,4HROC ,4H    ,3,    4HENDI,4HNSER,4HT   ,3,
     *            4HGLOB,4HAL  ,4H    ,3,    4HLOCA,4HL   ,4H    ,3,
     *            8*0/
      DATA INVLDC/4H'   ,1155,
     *            4H-   ,1155,
     *            4H<   ,1155,
     *            4H>   ,1155,
     *            4H(   ,1155,
     *            4H)   ,1155,
     *            4H$   ,15015,
     *            4H,   ,15015,
     *            4H    ,15015,
     *            2*0/
      DATA INVFCH/4H.   ,1155,
     *            4H@   ,1155,
     *            6*0/
      DATA IBLNK/4H    /
C
C
      IF (IDTYPE.EQ.5) THEN
         IOPNUM=1
         CALL FSTWHR (ID,IOPNUM,OLDOPN,IOLDOP)
         ELSE
            IOPNUM=-1
            CALL FSTWHR ('FCIDCK  ',IOPNUM,OLDOPN,IOLDOP)
         ENDIF
      
      LDEBUG=0
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IPR,*)
     *      ' NCHAR=',NCHAR,
     *      ' IPACKD=',IPACKD,
     *      ' IDTYPE=',IDTYPE,
     *      ' IPRINT=',IPRINT,
     *      ' '
         WRITE (IPR,'(1X,''ID='',2A4)') (ID(I),I=1,NCHAR/4)
         ENDIF
C
      TYPMSG='ERROR'
C      
      RESVRD=.FALSE.
      INVCHR=.FALSE.
      INFCHR=.FALSE.
      TMCHAR=.FALSE.
C
      MXTYPE=5
      NRSVWD=18
      NINVLC=9
      NINVFC=2
C
      DO 10 I=1,3
         PACKWD(I)=IBLNK
10       CONTINUE
C
      DO 20 I=1,12
         UNPKWD(I)=IBLNK
20       CONTINUE
C
      JPRINT=IPRINT
C
      IF (IPRINT.EQ.0.OR.IPRINT.EQ.1) THEN
         ELSE
            WRITE (IPR,190) IPRINT
            CALL WARN
            JPRINT=1
         ENDIF
C
      IF (IDTYPE.GT.0.AND.IDTYPE.LE.MXTYPE) THEN
         ELSE
            ISTAT=6
            IF (JPRINT.EQ.1) THEN
               WRITE (IPR,200) IDTYPE,MXTYPE
               CALL ERROR
               ENDIF
         ENDIF
C
      IF (IPACKD.EQ.0.OR.IPACKD.EQ.1) THEN
         ELSE
            ISTAT=6
            IF (JPRINT.EQ.1) THEN
               WRITE (IPR,210) IPACKD
               CALL ERROR
               ENDIF
         ENDIF
C
      IF (NCHAR.GT.0) THEN
         ELSE
            ISTAT=6
            IF (JPRINT.EQ.1) THEN
               WRITE (IPR,220) NCHAR
               CALL ERROR
               ENDIF
         ENDIF
C
      IF (ISTAT.GT.0) GO TO 180
C
      JCHAR=NCHAR
      IF (MXCHAR(IDTYPE).LT.NCHAR) JCHAR=MXCHAR(IDTYPE)
      IF (JCHAR.NE.NCHAR) TMCHAR=.TRUE.
      NWRDIN=(JCHAR-1)/4+1
C
      IF (IPACKD.EQ.1) GO TO 40
C
C  IDENTIFIER IS NOT PACKED - COPY INTO UNPKWD AND PACK INTO PACKWD
      DO 30 I=1,JCHAR
         UNPKWD(I)=ID(I)
30       CONTINUE
      CALL UPACK1 (ID,PACKWD,JCHAR)
      GO TO 50
C
C  IDENTIFIER IS PACKED - UNPACK INTO UNPKWD AND PACK INTO PACKWD
40    CALL UNPAKS (ID,UNPKWD,NWRDIN,MXCHAR(IDTYPE),ISTAT)
      CALL UPACK1 (UNPKWD,PACKWD,JCHAR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR ALL BLANKS
50    DO 60 J=1,NWRDIN
         IF (PACKWD(J).EQ.IBLNK) GO TO 60
         GO TO 70
60       CONTINUE
      ISTAT=1
      IF (JPRINT.EQ.0) GO TO 180
      WRITE (IPR,230)
     *   CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE)))
      CALL ERROR
      GO TO 180
C
C  CHECK IF FIRST CHARACTER IS BLANK
70    IF (UNPKWD(1).EQ.IBLNK) THEN
         ISTAT=2
         IF (JPRINT.EQ.0) GO TO 180
         WRITE (IPR,240)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,12)
         CALL ERROR
         GO TO 180
         ENDIF
C
      IVALUE=IVAL(IDTYPE)
C
C  CHECK FOR INVALID WORDS
      DO 100 I=1,NRSVWD
         IF (MOD(RSVWRD(4,I),IVALUE).NE.0) GO TO 100
            DO 90 J=1,NWRDIN
               IF (PACKWD(J).EQ.RSVWRD(J,I)) GO TO 90
               GO TO 100
90             CONTINUE
            RESVRD=.TRUE.
            GO TO 140
100      CONTINUE
C
C  FIND LAST NON-BLANK CHARACTER
      MCHAR=JCHAR
      DO 115 J=JCHAR,1,-1
         IF (UNPKWD(J).NE.IBLNK) THEN
            MCHAR=J
            GO TO 117
            ENDIF
115      CONTINUE
C
C  CHECK FOR INVALID CHARACTERS
117   DO 120 I=1,NINVLC
         IF (MOD(INVLDC(2,I),IVALUE).NE.0) GO TO 120
         DO 110 J=1,MCHAR
            IF (UNPKWD(J).EQ.INVLDC(1,I)) THEN
               INVCHR=.TRUE.
               INVCA4=INVLDC(1,I)
               CALL SUBSTR (INVLDC(1,I),1,1,CHAR1,1)
               IF (IDTYPE.EQ.5.AND.CHAR1.EQ.' ') THEN
                  TYPMSG='WARNING'
                  ELSE
                     TYPMSG='ERROR'
                  ENDIF
               WRITE (IPR,260) TYPMSG(1:LENSTR(TYPMSG)),
     *            CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *            INVCA1(1),J
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL ERROR
                  ISTAT=2
                  ELSE
                     CALL WARN
                  ENDIF
               ENDIF
110         CONTINUE
120      CONTINUE
C
C  CHECK FOR INVALID FIRST CHARACTER
      DO 130 I=1,NINVFC
         IF (MOD(INVFCH(2,I),IVALUE).NE.0) GO TO 130
         IF (UNPKWD(1).EQ.INVFCH(1,I)) THEN
            INFCHR=.TRUE.
            INVCA4=INVFCH(1,I)
            GO TO 140
         ENDIF
130      CONTINUE
C
140   IF (TMCHAR.OR.RESVRD.OR.INVCHR.OR.INFCHR) GO TO 150
      GO TO 180
C
150   IF (TMCHAR) GO TO 160
C
C  CHECK IF A RESERVED WORD
      IF (RESVRD) ISTAT=1
C
C  CHECK IF INVALID CHARACTER FOUND
      IF (INVCHR.OR.INFCHR) ISTAT=2
      GO TO 170
C
C  TOO MANY CHARACTERS IN ID
160   ISTAT=3
C
C  CHECK IF RESERVED WORD
      IF (RESVRD) ISTAT=4
C
C  CHECK IF INVALID CHARACTER FOUND
      IF (INVCHR.OR.INFCHR) ISTAT=5
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF TO PRINT MESSAGE
C
170   IF (JPRINT.EQ.0) GO TO 180
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,250)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE)))
         MCHAR=MXCHAR(IDTYPE)
         WRITE (IPR,330) MCHAR,
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,MCHAR)
         CALL ERROR
         GO TO 180
         ENDIF
      IF (ISTAT.EQ.2) THEN
         IF (INVCHR) WRITE (IPR,270) TYPMSG(1:LENSTR(TYPMSG)),
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE)))
         IF (INFCHR) WRITE (IPR,280)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      INVCA1(1)
         MCHAR=MXCHAR(IDTYPE)
         WRITE (IPR,330) MCHAR,
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,MCHAR)
         IF (TYPMSG.EQ.'ERROR') CALL ERROR
         IF (TYPMSG.EQ.'WARNING') THEN
            CALL WARN
            ISTAT=0
            ENDIF
         GO TO 180
         ENDIF
      IF (ISTAT.EQ.3) THEN
         WRITE (IPR,290)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      MXCHAR(IDTYPE),
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE)))
         MCHAR=MXCHAR(IDTYPE)
         WRITE (IPR,330) MCHAR,
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,MCHAR)
         CALL WARN
         GO TO 180
         ENDIF
      IF (ISTAT.EQ.4) THEN
         WRITE (IPR,300)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      MXCHAR(IDTYPE)
         MCHAR=MXCHAR(IDTYPE)
         WRITE (IPR,330) MCHAR,
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,MCHAR)
         CALL ERROR
         GO TO 180
         ENDIF
      IF (ISTAT.EQ.5) THEN
         IF (INVCHR) WRITE (IPR,310)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      MXCHAR(IDTYPE),INVCA1(1)
         IF (INFCHR) WRITE (IPR,320)
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      INVCA1(1)
         MCHAR=MXCHAR(IDTYPE)
         WRITE (IPR,330) MCHAR,
     *      CDTYPE(IDTYPE)(1:LENSTR(CDTYPE(IDTYPE))),
     *      (UNPKWD(I),I=1,MCHAR)
         CALL ERROR
         GO TO 180
         ENDIF
      WRITE (IPR,340) ISTAT
      CALL ERROR
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   IF (LDEBUG.GT.0) THEN
         WRITE (IPR,*)
     *      ' ISTAT=',ISTAT,
     *      ' '
         ENDIF
C      
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
190   FORMAT ('0**WARNING** IN FCIDCK - VALUE OF ARGUMENT IPRINT (',I3,
     *   ') MUST BE 0 OR 1. 1 ASSUMED.')
200   FORMAT ('0**ERROR** IN FCIDCK - VALUE OF ARGUMENT IDTYPE (',I3,
     *   ') MUST BE FROM 1 TO ',I3,'.')
210   FORMAT ('0**ERROR** IN FCIDCK - VALUE OF ARGUMENT IPACKD (',I3,
     *   ') MUST BE 0 OR 1.')
220   FORMAT ('0**ERROR** IN FCIDCK - VALUE OF ARGUMENT NCHAR(',I3,
     *   ' MUST BE GREATER THAN OR EQUAL TO ZERO.')
230   FORMAT ('0**ERROR** ',A,' IS ALL BLANKS.')
240   FORMAT ('0**ERROR** THE FIRST CHARACTER OF THE ',A,
     *   ' (',12A1,') IS A BLANK.')
250   FORMAT ('0**ERROR** ',A,' IS A RESERVED WORD.')
260   FORMAT ('0**',A,'** ',A,' ',
     *   'CONTAINS AN INVALID CHARACTER (',A1,') ',
     *   'IN POSITION ',I2,'.')
270   FORMAT ('0**',A,'** ',A,' ',
     *   'CONTAINS ONE OR MORE INVALID CHARACTERS.')
280   FORMAT ('0**ERROR** FIRST CHARACTER OF ',A,'(',A1,
     *   ') IS INVALID.')
290   FORMAT ('0**WARNING** ',A,' IS TOO LONG - ',
     *   'FIRST ',I2,' CHARACTERS ARE A VALID ',A,'.')
300   FORMAT ('0**ERROR** ',A,' IS TOO LONG - ',
     *   'FIRST ',I2,' CHARACTERS ARE A RESERVED WORD.')
310   FORMAT ('0**ERROR** ',A,' IS TOO LONG - ',
     *   'FIRST ',I2,' CHARACTERS ',
     *   'CONTAIN AN INVALID CHARACTER (',A1,').')
320   FORMAT ('0**ERROR** ',A,' IS TOO LONG - FIRST CHARACTER ',
     *   '(',A1,') IS INVALID.')
330   FORMAT (11X,'THE FIRST ',I2,' CHARACTERS OF ',A,' ARE : ',12A1)
340   FORMAT ('0**ERROR** IN FCIDCK - VALUE OF ISTAT (',I2,
     *   ') NOT RECOGNIZED.')
C
      END
