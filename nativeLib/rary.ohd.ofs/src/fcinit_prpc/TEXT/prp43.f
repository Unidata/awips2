C     MODULE  FCPRP43     VERSION 1       
c
c  =====================================================================
c  pgm:  prp43 (po)
c
c   in: po     .... parameter array
c  =====================================================================
      SUBROUTINE PRP43(PO)
c
C#######################################################################

C  THIS ROUTINE PRINTS OUT PARAMETRIC DATA FOR THE API-HFD
C  OPERATION STORED IN THE PO ARRAY BY THE PIN43 SUBROUTINE.

C#######################################################################
c  Initially written by
c     Ken Mack    NERFC                         August 1995
c     Tim Sweeney  HRL                          October 1995
c#######################################################################
c
C  CONTENTS OF THE PO ARRAY:

C     WORD    NAME       DESCRIPTION                            UNITS
C  ________   _______    _______________________________________________
C      1      VERS       VERSION NUMBER
C    2 - 3    RID        RUNOFF ZONE ID
C    4 - 8    RNAME      RUNOFF ZONE NAME
C      9      IRNUM      RUNOFF ZONE NUMBER
C     10      RLAT       LATITUDE OF RUNOFF ZONE CENTROID       DEG DEC
C     11      RLNG       LONGITUDE OF RUNOFF ZONE CENTROID      DEG DEC
C     12      RFCTR      RUNOFF ADJUSTMENT FACTOR
C     13      R24        24-HOUR API RECESSION FACTOR
C     14      PMAX       MAX LIMIT FOR NEW STORM RAIN/MELT      INCHES
C     15      ULIMW      UPPER ATI LIMIT (WINTER CURVE)         INCHES
C     16      BLIMW      BOTTOM ATI LIMIT (WINTER CURVE)        INCHES
C     17      ULIMS      UPPER ATI LIMIT (SUMMER CURVE)         INCHES
C     18      BLIMS      BOTTOM ATI LIMIT (SUMMER CURVE)        INCHES
C     19      TBAR       ACTUAL AVG ANNUAL BASIN TEMPERATURE    DEG FAHR
C     20      TTBAR      CORRECTED ANNUAL AVERAGE BASIN TEMPERATURE
c  21 - 22   EMPTY
C     23      NREL       GEOGRAPHICAL RELATIONSHIP NUMBER
C     24      IDELTA     COMPUTATIONAL TIME STEP                HOURS
C     25      NSW        NEW STORM WINDOW                       HOURS
C     26      NSPER      NUMBER OF PERIODS IN NSW
C     27      IUSEC      NUMBER OF WORDS NEEDED IN CO ARRAY
C     28      IOFAAA     I/O FLAG FOR API, ATI & FI TIME SERIES
C     29      ICOF       CARRYOVER INPUT FLAG
C   30 - 31   EMPTY
C     32      LMTS       LOCATION OF MANDATORY TIME SERIES INFORMATION
C                        IN PO ARRAY
C     33      LWKT       LOCATION OF WEEKLY BASIN TEMPERATURES IN PO
C     34      LOTS       LOCATION OF OPTIONAL TIME SERIES INFORMATION
C                        IN PO ARRAY
C
C LMTS - +1   TSIDRM     TIME SERIES ID FOR RAIN/MELT
C   LMTS+2    DTCRM      DATA TYPE CODE FOR RAIN/MELT
C   +3 - +4   TSIDRO     TIME SERIES ID FOR RUNOFF
C   LMTS+5    DTCRO      DATA TYPE CODE FOR RUNOFF
C
C LWKT - +51  MATI       WEEKLY BASIN TEMPERATURES
C
C LOTS - +1   TSIDAP     TIME SERIES ID FOR API
C   LOTS+2    DTCAP      DATA TYPE CODE FOR API
C   +3 - +4   TSIDAT     TIME SERIES ID FOR ATI
C   LOTS+5    DTCAT      DATA TYPE CODE FOR ATI
C   +6 - +7   TSIDRI     TIME SERIES ID FOR RI
C   LOTS+8    DTCRI      DATA TYPE CODE FOR RI
C#######################################################################

      include 'common/ionum'
      include 'common/fdbug'
c
c      COMMON /IONUM/ IN,IPR,IPU
c      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
c
      DIMENSION PO(*),SUBNAM(2),RID(2),RNAME(5),TSIDRM(2),
     1          TSIDAT(2),TSIDRO(2),TSIDRI(2),TSIDAP(2),TSIDTB(2)
      DIMENSION MATI(52)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp43.f,v $
     . $',                                                             '
     .$Id: prp43.f,v 1.1 1996/03/21 16:01:05 page Exp $
     . $' /
C    ===================================================================
C

      DATA SUBNAM/4HPRP4,4H3   /,NOP/43/,EMPTY/4H    /

C  CALL DEBUG CHECK ROUTINE

      CALL FPRBUG(SUBNAM,1,NOP,IFDEB)

C  PULL VARIABLES FROM THE PO ARRAY

      VERS     = PO(1)
      RID(1)   = PO(2)
      RID(2)   = PO(3)
      RNAME(1) = PO(4)
      RNAME(2) = PO(5)
      RNAME(3) = PO(6)
      RNAME(4) = PO(7)
      RNAME(5) = PO(8)
      IRNUM    = PO(9)
      RLAT     = PO(10)
      RLNG     = PO(11)
      RFCTR    = PO(12)
      R24      = PO(13)
      PMAX     = PO(14)
      ULIMW    = PO(15)
      BLIMW    = PO(16)
      ULIMS    = PO(17)
      BLIMS    = PO(18)
      TBAR     = PO(19)
      TTBAR    = PO(20)
c
      NREL     = PO(23)
      IDELTA   = PO(24)
      idel24   = 24
      NSW      = PO(25)
      NSPER    = PO(26)
      IUSEC    = PO(27)
      IOFAAA   = PO(28)
      ICOF     = PO(29)
C
      LMTS = PO(32) + 0.01
      LWKT = PO(33) + 0.01
      LOTS = PO(34) + 0.01
C
      TSIDRM(1) = PO(LMTS)
      TSIDRM(2) = PO(LMTS+1)
      DTCRM     = PO(LMTS+2)
      TSIDRO(1) = PO(LMTS+3)
      TSIDRO(2) = PO(LMTS+4)
      DTCRO     = PO(LMTS+5)
C
      DO 200 I=1,52
200   MATI(I) = PO(LWKT+I-1)
C
      TSIDAP(1) = PO(LOTS)
      TSIDAP(2) = PO(LOTS+1)
      DTCAP     = PO(LOTS+2)
      TSIDAT(1) = PO(LOTS+3)
      TSIDAT(2) = PO(LOTS+4)
      DTCAT     = PO(LOTS+5)
      TSIDRI(1) = PO(LOTS+6)
      TSIDRI(2) = PO(LOTS+7)
      DTCRI     = PO(LOTS+8)

C   NOW PRINT OUT THESE VARIABLES.

      WRITE(IODBUG,1000)RNAME
      WRITE(IODBUG,1010)VERS
      WRITE(IODBUG,1020)RID
      WRITE(IODBUG,1030)RNAME
      WRITE(IODBUG,1040)IRNUM
      WRITE(IODBUG,1050)RLAT
      WRITE(IODBUG,1060)RLNG
      WRITE(IODBUG,1070)RFCTR
      WRITE(IODBUG,1080)R24
      WRITE(IODBUG,1090)PMAX
      WRITE(IODBUG,1100)ULIMW
      WRITE(IODBUG,1110)BLIMW
      WRITE(IODBUG,1120)ULIMS
      WRITE(IODBUG,1130)BLIMS
      WRITE(IODBUG,1135)TTBAR
      WRITE(IODBUG,1142) NREL
      WRITE(IODBUG,1150)IDELTA
      WRITE(IODBUG,1160)NSW
      WRITE(IODBUG,1170)NSPER
      WRITE(IODBUG,1180)IUSEC
      WRITE(IODBUG,1190)IOFAAA
      WRITE(IODBUG,1200)ICOF
      WRITE(IODBUG,1300)
      WRITE(IODBUG,1310)TSIDRM,DTCRM,IDELTA
      WRITE(IODBUG,1320)TSIDRO,DTCRO,IDELTA
      IF(TSIDAP(1)-EMPTY)340,350,340
340   WRITE(IODBUG,1340)TSIDAP,DTCAP,IDELTA
350   IF(TSIDAT(1)-EMPTY)360,370,360
360   WRITE(IODBUG,1360)TSIDAT,DTCAT,IDEL24
370   IF(TSIDRI(1)-EMPTY)380,400,380
380   WRITE(IODBUG,1380)TSIDRI,DTCRI,IDELTA
400   WRITE(IODBUG,1410) (MATI(I),I=1,52)
9999  RETURN
c
1000  FORMAT(//15X,'API-HFD PARAMETER VALUES FOR ',5A4,
     1      //9X,'INTERNAL',/9X,'VARIABLE',/,11X,'NAME',
     2      7X,'DESCRIPTION',30X,'CONTENTS',/)
1010  FORMAT(11X,'VERS ',5('.'),' API-HFD VERSION NUMBER ',
     1      17('.'),1X,F5.2)
1020  FORMAT(11X,'RID   ',4('.'),' 8-LETTER RUNOFF ZONE I.D. ',
     1      14('.'),1X,2A4)
1030  FORMAT(11X,'RNAME ',4('.'),' 20-LETTER RUNOFF ZONE NAME ',
     1      13('.'),1X,5A4)
1040  FORMAT(11X,'IRNUM ',4('.'),' RUNOFF ZONE NUMBER ',
     1      21('.'),1X,I4)
1050  FORMAT(11X,'RLAT  ',4('.'),' LATITUDE OF R.O. ZONE CENTROID ',
     1      '(DEG DEC) ',1X,F5.2)
1060  FORMAT(11X,'RLNG  ',4('.'),' LONGITUDE OF R.O. ZONE CENTROID ',
     1      '(DEG DEC)',1X,F5.2)
1070  FORMAT(11X,'RFCTR ',4('.'),' BASIN RUNOFF ADJUSTMENT FACTOR ',
     1      9('.'),1X,F5.2)
1080  FORMAT(11X,'R24   ',4('.'),' 24-HOUR API RECESSION FACTOR ',
     1      11('.'),1X,F5.3)
1090  FORMAT(11X,'PMAX  ',4('.'),' STORM BREAK RAIN/MELT CRITERION ',
     1      8('.'),1X,F5.2)
1100  FORMAT(11X,'ULIMW ',4('.'),' UPPER LIMIT FOR ATI ',
     1      '(WINTER CURVE) ',5('.'),1X,F5.1)
1110  FORMAT(11X,'BLIMW ',4('.'),' BOTTOM LIMIT FOR ATI ',
     1      '(WINTER CURVE) ',4('.'),1X,F5.1)
1120  FORMAT(11X,'ULIMS ',4('.'),' UPPER LIMIT FOR ATI ',
     1      '(SUMMER CURVE) ',5('.'),1X,F5.1)
1130  FORMAT(11X,'BLIMS ',4('.'),' BOTTOM LIMIT FOR ATI ',
     1      '(SUMMER CURVE) ',4('.'),1X,F5.1)
1135  FORMAT(11X,'TBAR  ',4('.'),' AVG ANNUAL BASIN TEMP',19('.'),
     1      1X,F5.1)
1142  FORMAT(11X,'NREL  ',4('.'),' GEOGRAPHICAL RELATIONSHIP ',
     1      'NUMBER',8('.'),1X,I4)
1150  FORMAT(11X,'IDELTA',4('.'),' COMPUTATIONAL TIME STEP INTERVAL ',
     1      '(HOURS)',1X,I4)
1160  FORMAT(11X,'NSW   ',4('.'),' NEW STORM WINDOW (HOURS) ',
     1      15('.'),1X,I4)
1170  FORMAT(11X,'NSPER ',4('.'),' NUMBER OF PERIODS IN NSW ',
     1      15('.'),1X,I4)
1180  FORMAT(11X,'IUSEC ',4('.'),' NUMBER OF WORDS USED IN ',
     1      'THE CO ARRAY ',3('.'),1X,I4)
1190  FORMAT(11X,'IOFAAA',4('.'),' I/O FLAG FOR API, ATI & FI TIME ',
     1      'SERIES ',2X,I4,/22X,'(0 = DONT SAVE AS TS, ',
     2      '1 = SAVE AS TS)')
1200  FORMAT(11X,'ICOF  ',4('.'),' CARRYOVER INPUT FLAG ',19('.'),
     1      1X,I4,/22X,'(0 = USE DEFAULTS, 1 = READ INPUT)')
1300  FORMAT(//18X,'TIME SERIES USED BY THE API-HFD OPERATION:',
     1      //4X,'TS I.D.',7X,'TYPE',6X,'DESCRIPTION',23X,
     2      'TIME INTERVAL',/)
1310  FORMAT(4X,2A4,6X,A4,6X,'RAINFALL/MELT',21X,I4,' HOURS')
1320  FORMAT(4X,2A4,6X,A4,6X,'RUNOFF',28X,I4,' HOURS')
1340  FORMAT(4X,2A4,6X,A4,6X,'STORM API',25X,I4,' HOURS')
1360  FORMAT(4X,2A4,6X,A4,6X,'ANTECEDENT TEMPERATURE INDEX',6X,I4,
     1       ' HOURS')
1380  FORMAT(4X,2A4,6X,A4,6X,'FINAL INDEX',23X,I4,' HOURS')
1410  FORMAT(/,2X,'WEEKLY BASIN TEMPERATURES:',4 (/,5X,13( 1X,I3) ) )
C
      END
