C  =====================================================================
C  pgm: SHOUT .. Output shef message data into the "shefout" binary file
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
C  rqd: SHSAVU
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
C  =====================================================================
      SUBROUTINE SHOUT(ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,
     $                 KSE,PARCOD,IDUR,CODP,VALU,IQUAL,IREV,JID,ITIME,
     $                 QUO,IERR)

      EXTERNAL           SHSAVU

      CHARACTER*(*)      QUO
      CHARACTER*8        ID,JID,PARCOD
      CHARACTER*4        BLNK4
      CHARACTER*3        BLNK3
      CHARACTER*1        IQUAL
      INTEGER            IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE
      INTEGER            IDUR,IREV,ITIME,IERR
      REAL               CODP
      DOUBLE PRECISION   VALU

      INTEGER            INITZ,LUNO

      SAVE        INITZ,LUNO
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shout.f,v $
     . $',                                                             '
     .$Id: shout.f,v 1.4 1997/12/31 20:33:36 page Exp $
     . $' /
C    ===================================================================
C

      DATA   INITZ,BLNK4,BLNK3,LUNO / 0, '    ', '    ', -1 /

C                   On first pass, get and save unit-no, format-type
 
      IF (INITZ .EQ. 0) THEN
       INITZ = 1
       CALL SHSAVU('G_SHEFOUT   ',LUNO)
      ENDIF

C        Uncomment the statements below if 2-digit years are needed

CC    IYR = IYR - ( (IYR/100)*100 )
CC    KYR = KYR - ( (KYR/100)*100 )

C                   Write data to "shefout" file

      IF (LUNO.GE.0 .AND. LUNO.LE.99) THEN
        WRITE(LUNO,IOSTAT=IERR)
     $        ID,IYR,IMO,IDA,IHR,IMN,ISE,KYR,KMO,KDA,KHR,KMN,KSE,
     $        PARCOD(1:1),BLNK3,PARCOD(2:2),BLNK3,IDUR,
     $        PARCOD(4:4),BLNK3,PARCOD(5:5),BLNK3,PARCOD(6:6),BLNK3,
     $        CODP,VALU,IQUAL,BLNK3,
     $        IREV,JID,ITIME,PARCOD,BLNK4,BLNK4,
     $        QUO
      ENDIF

      RETURN
      END
