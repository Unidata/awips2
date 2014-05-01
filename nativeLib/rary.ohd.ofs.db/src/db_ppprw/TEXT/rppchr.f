C MEMBER PPRPPCHR
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.10:13:16 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPPCHR (MONTH,NPXCH,PXCH,NFILL,ISTAT)
C
C          ROUTINE:  RPPCHR
C
C             VERSION:  1.0.0
C
C                DATE:  12-9-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL READ THE CHARACTERISTICS FOR ALL STATIONS
C    FROM THE PREPROCESSOR PARAMETRIC DATA BASE
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       MONTH        I    I    1    THE MONTH (1-12) DESIRED
C      NPXCH        I     I    1    THE NUMBER OF I*2 WORDS IN PXCH
C       PXCH        I*2  O  NPXCH   CHARACTERISTICS, FOR ALL STAS
C       NFILL       I    O     1    NUMBER OF WORDS IN PXCH THAT ARE FIL
C       ISTAT       I    O     1    STATUS, 0=OK, 1=SYSTEM EERROR
C                                             2=MONTH INVALID
C                                        3=PXCH TOO SMALL, NFILL WORDS
C                                                NEEDED
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 PXCH(1),WORK(32)
      DIMENSION ICNTL(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rppchr.f,v $
     . $',                                                             '
     .$Id: rppchr.f,v 1.1 1995/09/17 18:45:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,100)
C
      ISTAT=0
      ISTAT1=0
C
      NFILLW=0
      LREC2=LRECPP*2
C
      IDXDAT=IPCKDT('CHAR')
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK THAT CHARS IS DEFINED
      IF (ICREC.NE.0) GO TO 10
      IF (IPPDB.GT.0) WRITE (LPE,110)
      NFILL=0
      GO TO 90
C
C  READ CONTROL RECORD
10    LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
C
      IF (IPPDB.GT.0) WRITE (IOGDB,120) ICNTL
C
C  CHECK IF ENOUGH ROOM IN ARRAY
      NFILL=ICNTL(9)
      IF (NFILL.LT.1) GO TO 90
      NFILLW=NFILL
      IF (NPXCH.GE.NFILLW) GO TO 20
      ISTAT1=3
      NFILLW=NPXCH
C
C  CHECK FOR A VALID MONTH
20    IF (MONTH.GE.1.AND.MONTH.LE.12) GO TO 30
      ISTAT=2
      GO TO 90
C
C  COMPUTE RECORD INFORMATION
30    IREC=(MONTH-1)*ICNTL(3)+ICNTL(2)
      NRECM1=IUNRCD(NFILLW,LREC2)-1
      NUM=NRECM1*LREC2
      IF (NUM.LE.NPXCH) GO TO 40
      NRECM1=NPXCH/LREC2
      NUM=NRECM1*LREC2
C
40    IF (IPPDB.GT.0) WRITE (IOGDB,130) IREC,NRECM1,NUM
50    IF (NRECM1.LT.1) GO TO 60
C
C  READ RECORDS
      CALL RVLRCD (LUFILE,IREC,NRECM1,PXCH,LRECPP,ISTAT)
      IF (ISTAT.NE.0) GO TO 80
C
C  CHECK IF THERE IS ONE MORE TO READ
60    IF (NFILLW.LE.NUM) GO TO 90
      NUMLFT=NFILLW-NUM
      IREC=IREC+NRECM1
      CALL UREADT (LUFILE,IREC,WORK,ISTAT)
      IF (IPPDB.GT.0) WRITE (IOGDB,140) IREC,NUMLFT
      N=NUM+1
      DO 70 I=1,NUMLFT
         PXCH(N)=WORK(I)
         N=N+1
70       CONTINUE
      GO TO 90
C
C  READ ERROR
80    IF (IPPDB.GT.0) WRITE (LPE,150) LUFILE,ISTAT
C
90    IF (ISTAT1.NE.0) ISTAT=ISTAT1
C
      IF (IPPTR.GT.0) WRITE (IOGDB,160) MONTH,NFILLW,ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' ENTER RPPCHR')
110   FORMAT (' **NOTE** IN RPPCHR - CHARACTERISTICS NOT DEFINED.')
120   FORMAT (' IN RPPCHR - CHAR CONTROL=',4I4,2X,A4,2X,11I4)
130   FORMAT (' IN RPPCHR - FIRST RECORD=',I6,' NRECM1=',I4,' NUM=',I6)
140   FORMAT (' IN RPPCHR - IREC=',I6,' NUMLFT=',I6)
150   FORMAT (' **ERROR** IN RPPCHR - READING FROM FILE ',I3,
     *     ' ISTAT=',I5)
160   FORMAT (' EXIT RPPCHR - MONTH=',I4,
     *   ' NFILLW=',I6,
     *   ' ISTAT=',I3)
C
      END
