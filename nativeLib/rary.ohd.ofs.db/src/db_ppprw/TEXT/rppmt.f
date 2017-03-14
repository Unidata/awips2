C MEMBER RPPMT
C  (from old member PPRPPMT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.10:13:23 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE RPPMT (MONTH,NTMP,MAXTMP,MINTMP,NFILL,ISTAT)
C
C          ROUTINE:  RPPMT
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
C    THIS ROUTINE WILL READ THE MEAN MONTHLY TEMPS FOR ALL STATIONS
C    FOR ONE MONTH FROM THE PREPROCESSOR PARAMETRIC DATA BASE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       MONTH        I    I    1    THE MONTH (1-12) DESIRED
C       NTMP        I     I    1    THE NUMBER OF I*2 WORDS IN MAXTMP
C       MAXTMP      I*2  O  NTMP    MEAN MONTHLY MAX TEMPS FOR ALL STA
C       MINTMP     I*2   O   NTMP   MEAN MONTHLY MIN TEMPS
C       NFILL       I    O     1    NUMBER OF WORDS FILLED IN MAXTMP
C       ISTAT       I    O     1    STATUS, 0=OK, 1=SYSTEM ERROR
C                                      2=MONTH INVALID
C                                      3=MAXTMP TOO SMALL, NFILL WORDS
C                                        FILLED
C
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
      INTEGER*2 MAXTMP(1),MINTMP(1),WORK(32)
      DIMENSION ICNTL(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/rppmt.f,v $
     . $',                                                             '
     .$Id: rppmt.f,v 1.1 1995/09/17 18:45:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPPTR.EQ.1) WRITE (IOGDB,120)
C
      ISTAT=0
      ISTAT1=0
C
      NFILLW=0
      LREC2=LRECPP*2
C
      IDXDAT=IPCKDT('MMMT')
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK THAT MMMTS IS DEFINED
      IF (ICREC.NE.0) GO TO 10
      IF (IPPDB.EQ.1) WRITE (LPE,130)
      NFILL=0
      GO TO 110
C
C  READ CONTROL RECORD
10    LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 100
C
      IF (IPPDB.EQ.1) WRITE (IOGDB,140) ICNTL
C
C  CHECK IF ENOUGH ROOM IN ARRAY
      NFILL=ICNTL(9)
      IF (NFILL.LT.1) GO TO 110
      NFILLW=NFILL
      IF (NTMP.GE.NFILL) GO TO 20
      ISTAT1=3
      NFILLW=NTMP
C
C CHECK FOR A VALID MONTH
20    IF (MONTH.GE.1.AND.MONTH.LE.12) GO TO 30
      ISTAT=2
      GO TO 110
C
C  COMPUTE RECORD INFORMATION
30    IREC=(MONTH-1)*2*ICNTL(3)+ICNTL(2)
      NRECM1=IUNRCD(NFILLW,LREC2)-1
      NUM=NRECM1*LREC2
      IF (NUM.LE.NTMP) GO TO 40
      NRECM1=NTMP/LREC2
      NUM=NRECM1*LREC2
C
40    IF (IPPDB.EQ.1) WRITE (IOGDB,150) IREC,NRECM1,NUM
C
C  READ MAX TEMPS
C
      IF (NRECM1.LT.1) GO TO 50
C
C  READ RECORDS
      CALL RVLRCD (LUFILE,IREC,NRECM1,MAXTMP,LRECPP,ISTAT)
      IF (ISTAT.NE.0) GO TO 100
C
C  CHECK IF THERE IS ONE MORE TO READ
50    IF (NFILLW.LE.NUM) GO TO 70
      NUMLFT=NFILLW-NUM
      IREC1=IREC+NRECM1
      CALL UREADT (LUFILE,IREC1,WORK,ISTAT)
      IF (IPPDB.EQ.1) WRITE (IOGDB,160) IREC1,NUMLFT
      N=NUM+1
      DO 60 I=1,NUMLFT
         MAXTMP(N)=WORK(I)
         N=N+1
60       CONTINUE
C
C  READ MIN TEMPS
C
70    IREC=IREC+ICNTL(3)
C
      IF (NRECM1.LT.1) GO TO 80
C
C  READ RECORDS
      CALL RVLRCD (LUFILE,IREC,NRECM1,MINTMP,LRECPP,ISTAT)
      IF (ISTAT.NE.0) GO TO 100
C
C  CHECK IF THERE IS ONE MORE TO READ
80    IF (NFILLW.LE.NUM) GO TO 110
      NUMLFT=NFILLW-NUM
      IREC=IREC+NRECM1
      CALL UREADT (LUFILE,IREC,WORK,ISTAT)
      IF (IPPDB.EQ.1) WRITE (IOGDB,170) IREC,NUMLFT
      N=NUM+1
      DO 90 I=1,NUMLFT
         MINTMP(N)=WORK(I)
         N=N+1
90       CONTINUE
      GO TO 110
C
C  READ ERROR
100   IF (IPPDB.EQ.1) WRITE (LPE,180) LUFILE,ISTAT
110   IF (ISTAT1.NE.0) ISTAT=ISTAT1
      IF (IPPTR.GT.0) WRITE (IOGDB,190) MONTH,NFILLW,ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' ENTER RPPMT')
130   FORMAT (' **NOTE** IN RPPMT - MEAN MONTHLY TEMPS NOT DEFINED.')
140   FORMAT (' IN RPPMT - MMMT CONTROL=',4I4,2X,A4,2X,11I4)
150   FORMAT (' IN RPPMT - IREC=',I6,' NRECM1=',I4,' NUM=',I6)
160   FORMAT (' IN RPPMT - IREC1',I6,' MUMLNT=',I6)
170   FORMAT (' IN RPPMT - IREC',I6,' NUMLFT=',I6)
180   FORMAT (' **ERROR** IN RPPMT - READING FROM FILE ',I3,
     *     ' ISTAT=',I5)
190   FORMAT (' EXIT RPPMT - MONTH=',I4,
     *   ' NFILLW=',I6,
     *   ' ISTAT=',I3)
C
      END
