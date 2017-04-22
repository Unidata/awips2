C  =====================================================================
C  pgm: SHOUT .. Output filtered shef message as text using "shefit1"
C
C  use:     CALL SHOUT(ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,
C  use:    $           KSE,PARCOD,IDUR,CODP,VALU,IQUAL,IREV,JID,ITIME,
C  use:    $           QUO,IERR)
C
C   in: ID ........ station identification name - CHAR*8
C   in: IYR,IMO,IDA,IHR,IMN,ISE .. zulu date-time for data - INT
C   in: KYR,KMO,KDA,KHR,KMN,KSE .. zulu dte-tm for creation date - INT
C   in: PARCOD .... parameter code for data - CHAR*8
C   in: IDUR ...... duration code - INT
C   in: CODP ...... probability code - REAL
C   in: VALU ...... data value - DOUBLE PRECISION
C   in: IQUAL ..... data qualifier code - CHAR*1
C   in: IREV ...... if 1 then revision data, else 0 - INT
C   in: JID ....... group or source code, else blank - CHAR*8
C   in: ITIME ..... time series code (0=no ts,1=first,2=oth elem) - INT
C   in: QUO ....... quote about data value - CHAR*(*)
C   in: (subrtn) .. enter logical unit number outside this rtn with:
C   in:               CALL SHSAVU('P_SHEFOUT',<number>)
C  out: IERR ...... "IOSTAT" from write stmt, zero if no error - INT
C
C  rqd: SHSAVU,SHFMOT
C
C  cmt:  NAME      TYPE  I/O   DIM   DESCRIPTION
C  cmt:
C  cmt:  ID         A     I    *8    8 CHAR STATION ID
C  cmt:  IYR        I     I     1    YEAR OF OBSERVATION DATE(4 DIGITS)
C  cmt:  IMO        I     I     1    MONTH OF OBSERVATION DATE
C  cmt:  IDA        I     I     1    DAY OF OBSERVATION DATE
C  cmt:  IHR        I     I     1    HOUR OF OBSERVATION DATE (0-23)
C  cmt:  IMN        I     I     1    MINUTE OF OBSERVATION DATE
C  cmt:  ISE        I     I     1    SECOND OF OBSERVATION DATE
C  cmt:  KYR        I     I     1    YEAR OF CREATION DATE (4 DIGITS)
C  cmt:  KMO        I     I     1    MONTH OF CREATION DATE
C  cmt:  KDA        I     I     1    DAY OF CREATION DATE
C  cmt:  KHR        I     I     1    HOUR OF CREATION DATE
C  cmt:  KMN        I     I     1    MINUTE OF CREATION DATE
C  cmt:  KSE        I     I     1    SECOND OF CREATION DATE
C  cmt: PARCOD(1:1) A1    I     1    FIRST CHAR OF PHYSICAL ELEMENT CODE
C  cmt: PARCOD(2:2) A1    I     1    SECOND CHAR OF PE CODE
C  cmt:  IDUR       I     I     1    ENCODED DURATION CODE
C  cmt: PARCOD(4:4) A1    I     1    TYPE CODE
C  cmt: PARCOD(5:5) A1    I     1    SOURCE CODE
C  cmt: PARCOD(6:6) A1    I     1    EXTREMUM CODE
C  cmt:  CODP       R     I     1    CODE PROBABILITY
C  cmt:  VALU       R     I     1    DATA VALUE
C  cmt:  IQUAL      A     I    *1    DATA QUALIFIER
C  cmt:  IREV       I     I     1    REVISION CODE (0=not a rev,1=rev)
C  cmt:  JID        A     I    *8    DATA SOURCE
C  cmt:  ITIME      I     I     1    TIME SERIES INDICATOR
C  cmt:                              (0=no ts,1=first elem,2=othr elem)
C  cmt: PARCOD(1:8) A8    I    *8    FULL PARAMETER CODE
C  cmt: QUO(1:#)    A#    I    *#    QUOTE STRING (# may be 80 or so)
C
C  cmt: One of the following options can be given as the first argument:
C  cmt:     -1 ... Text output as one long line, full quotes (default)
C  cmt:     -2 ... Text output as two lines, quotes limited to 66 chars
C  cmt:     -b ... Binary output if output is to a file
C  =====================================================================
      SUBROUTINE SHOUT(ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,
     $                 KSE,PARCOD,IDUR,CODP,VALU,IQUAL,IREV,JID,ITIME,
     $                 QUO,IERR)

      INTRINSIC          LEN
      EXTERNAL           SHSAVU,SHFMOT

      CHARACTER*(*)      QUO
      CHARACTER*8        ID,JID,PARCOD
      CHARACTER*4        BLNK4
      CHARACTER*3        BLNK3
      CHARACTER*1        IQUAL
      INTEGER            IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE
      INTEGER            IDUR,IREV,ITIME,IERR,IE2
      INTEGER            LEN,IEND
      REAL               CODP
      DOUBLE PRECISION   VALU

      CHARACTER*1        FTYP
      INTEGER            INITZ,LUNO,LUER,IMAX

      SAVE        INITZ,LUNO,LUER,FTYP

      DATA   INITZ,BLNK4,BLNK3 / 0, '    ', '    ' /
      DATA   LUNO,LUER,FTYP    / -1, -1, 'D' /

C                   On first pass, get and save unit-no, format-type

      IF (INITZ .EQ. 0) THEN
       INITZ = 1
       CALL SHSAVU('G_SHEFOUT   ',LUNO)
       CALL SHSAVU('G_SHEFERROR ',LUER)
       CALL SHFMOT('GET_VALUE   ',FTYP)
      ENDIF
 
C                   Write data to "shefout" file
 
      IF (LUNO.GE.0 .AND. LUNO.LE.99) THEN
 
C  ----------------------------------
       IF (FTYP .EQ. 'B') THEN
C  ----------------------------------  Output format 'B' for binary
         WRITE(LUNO,IOSTAT=IERR)
     $         ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE,
     $         PARCOD(1:1),BLNK3,PARCOD(2:2),BLNK3,IDUR,
     $         PARCOD(4:4),BLNK3,PARCOD(5:5),BLNK3,PARCOD(6:6),BLNK3,
     $         CODP,VALU,IQUAL,BLNK3,
     $         IREV,JID,ITIME,PARCOD,BLNK4,BLNK4,
     $         QUO
C  ----------------------------------
       ELSEIF (FTYP .EQ. '2') THEN
C  ----------------------------------  Output format '2'
         WRITE(LUNO,120,IOSTAT=IERR)
     $     ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE,
     $     PARCOD(1:2),PARCOD(4:6),VALU,IQUAL,CODP,IDUR,IREV,JID,ITIME
  120    FORMAT(A8,I4,5I2,I5,5I2,1X,A2,1X,A3,
     $     F10.3,1X,A1,F6.2,I5,I2,1X,A8,I1)


         IEND = LEN(QUO)
  140    IF (IEND .LE. 0) GOTO 160
           IF (QUO(IEND:IEND) .NE. ' ') THEN
             IF (IEND .LE. 69) THEN
               WRITE(LUNO,'(8X,''"'',A,''"'')',IOSTAT=IERR) QUO(1:IEND)
             ELSE
               WRITE(LUNO,'(8X,''"'',A,''..."'')',IOSTAT=IERR) QUO(1:66)
             ENDIF
             IEND = 0
           ELSE
             IEND = IEND-1
           ENDIF
           GOTO 140
  160    CONTINUE
C  ----------------------------------
       ELSE
C  ----------------------------------  Default output ('D' or '1')
         IF (INITZ .EQ. 1) THEN
           INITZ = 2
           IF (LUNO .EQ. 6) WRITE(LUER,200,IOSTAT=IE2)
           IF (LUNO .EQ. 6) WRITE(LUER,210,IOSTAT=IE2)
  200      FORMAT('STATION   OBSERVATION-DATE/TM  CREATION-DATE/TIME',
     $            '   PARM-CD     DATA-VALUE Q  PROB-CD DURTN R T',
     $            '  GROUP-NM  QUOTE')
  210      FORMAT('aaaaaaaa  yyyy mm dd hh nn ss  yyyy mm dd hh nn ss',
     $            '  pedtsep     rrrrr.dddd a  rrr.ddd iiiii i i',
     $            '  aaaaaaaa  "aa..."',/)
         ENDIF
 
         IEND = LEN(QUO)
         IMAX = 1
  220    IF (IEND .LE.IMAX) GOTO 230
           IF (QUO(IEND:IEND) .NE. ' ') THEN
             IMAX = IEND
           ELSE
             IEND = IEND-1
           ENDIF
           GOTO 220
  230    CONTINUE
 
         WRITE(LUNO,240,IOSTAT=IERR)
     $     ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE,
     $     PARCOD,VALU,IQUAL,CODP,IDUR,IREV,ITIME,JID,QUO(1:IEND)
  240    FORMAT(A8,2X,I4.4,'-',I2.2,'-',I2.2,1X,I2.2,':',I2.2,':',I2.2,
     $             2X,I4.4,'-',I2.2,'-',I2.2,1X,I2.2,':',I2.2,':',I2.2,
     $             2X,A8,F14.4,1X,A1,F9.3,I6.4,I2,I2,2X,A8,'  "',A,'"')
C 240    FORMAT(A8,2X,I4,5I3,2X,I4,5I3,2X,A8,F14.4,1X,A1,F9.3,
C    $     I6,I2,I2,2X,A8,'  "',A,'"')
C  ----------------------------------
        ENDIF
C  ----------------------------------

      ENDIF

      RETURN
      END
