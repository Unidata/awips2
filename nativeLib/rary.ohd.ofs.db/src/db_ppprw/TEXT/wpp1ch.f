C MEMBER WPP1CH
C  (from old member PPWPP1CH)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/22/94.13:05:11 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPP1CH (PXCHR,IPTRCH,ISTAT)
C
C          ROUTINE:  WPP1CH
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
C    THIS ROUTINE WILL WRITE THE CHARACTERISTICS FOR ONE STATION
C    TO THE RESERVED CHARACTERISTIC RECORDS IN THE PPPDB
C    IT WRITES 12 VALUES, ONE FOR EACH MONTH.  UNUSED SLOTS CREATED
C    WHEN STATIONS ARE DELETED, ARE REUSED.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       PXCHR       R    I    12    CHARACTERISTICS, 1 PER MONTH
C       IPTRCH      I   I/O    1    SUBSCRIPT IN CHAR RECORD FOR THIS
C                                     STATION
C       ISTAT       I    O     1    STATUS, 0=OK, 1=SYSTEM ERROR
C                                             2=FILE IS FULL
C                                               3=IPTRCH TOO BIG
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
      DIMENSION PXCHR(12),ICNTL(16)
      INTEGER*2 WORK(32)
      INTEGER*2 NOCHAR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/wpp1ch.f,v $
     . $',                                                             '
     .$Id: wpp1ch.f,v 1.1 1995/09/17 18:45:18 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA LCHAR/4HCHAR/
      DATA NOCHAR/-9999/
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,150)
C
      ISTAT=0
C
C  COMPUTE LRECL FOR I*2 WORDS
      LREC2=LRECPP*2
C
      IDXDAT=IPCKDT(LCHAR)
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK IF CHARS IS DEFINED
      IF (ICREC.NE.0) GO TO 10
      IF (IPPDB.GT.0) WRITE (LPE,160)
      ISTAT=3
      GO TO 140
C
C  READ CONTROL RECORD
10    LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
C
      IF (IPPDB.GT.0) WRITE (IOGDB,170) LCHAR,ICNTL
C
      IF (ICNTL(5).EQ.LCHAR) GO TO 20
      IF (IPPDB.GT.0) WRITE (LPE,180) ICNTL
      ISTAT=1
      GO TO 140
C
C  CHECK IF REWRITING AN EXISITING STATION
20    IF (IPTRCH.GT.0.AND.IPTRCH.LE.ICNTL(9)) GO TO 50
C
C  CHECK IF THERE ARE UNUSED SLOTS
      IF (IPTRCH.GT.ICNTL(9)) GO TO 130
C
C  CHECK IF NUMBER OF STATIONS IS LESS THAN LAST USED SLOT
      IF (ICNTL(7).LT.ICNTL(9)) GO TO 60
C
C  NO EMPTY SLOTS - PUT IN NEXT AVAILABLE SLOT
30    IF (ICNTL(9).LT.ICNTL(8)) GO TO 40
      ISTAT=2
      GO TO 140
C
40    IPTRCH=ICNTL(9)+1
      ICNTL(7)=ICNTL(7)+1
C
50    IREC=IUNRCD(IPTRCH,LREC2)+ICNTL(2)-1
      JSLOT=IPTRCH-(IREC-ICNTL(2))*LREC2
      GO TO 100
C
C  CHECK FOR UNUSED SLOT
60    IREC=ICNTL(2)
      NREC=ICNTL(3)
      MN=LREC2
      LASTLP=ICNTL(9)-(NREC-1)*LREC2
      DO 80 I=1,NREC
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
         IF (I.EQ.NREC) MN=LASTLP
C     CHECK FOR EMPTY SLOT
         DO 70 JSLOT=1,MN
            IF (WORK(JSLOT).EQ.NOCHAR) GO TO 90
70          CONTINUE
         IREC=IREC+1
80        CONTINUE
C
C  NO EMPTY SLOT FOUND - RESET COUNTERS
      ICNTL(7)=ICNTL(9)
      GO TO 30
C
C  FOUND EMPTY SLOT
90    IPTRCH=JSLOT+(I-1)*LREC2
      ICNTL(7)=ICNTL(7)+1
      IF (IPPDB.GT.0) WRITE (IOGDB,190) IPTRCH
C
C  WRITE THE CHARACTERISTICS
100   DO 110 IMO=1,12
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
C     ENTER THE CHAR FOR THIS MONTH
         WORK(JSLOT)=PXCHR(IMO)*100.0+.5
         IF (IPPDB.GT.0) WRITE (IOGDB,200) IMO,IREC,WORK(JSLOT),
     *     PXCHR(IMO)
         CALL UWRITT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 120
         IREC=IREC+ICNTL(3)
110      CONTINUE
C
      IF (IPPDB.GT.0) WRITE (IOGDB,210) IPTRCH,WORK(JSLOT)
C
C  UPDATE CONTROL RECORD
      IF (ICNTL(9).LT.IPTRCH) ICNTL(9)=IPTRCH
      CALL UWRITT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.EQ.0) GO TO 140
C
C  READ/WRITE ERROR
120   IF (IPPDB.GT.0) WRITE (LPE,220) LUFILE,ISTAT
      GO TO 140
C
130   ISTAT=3
C
140   IF (IPPTR.GT.0) WRITE (IOGDB,230) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT (' ENTER WPP1CH')
160   FORMAT (' **NOTE** IN WPP1CH - CHARACTERISTICS NOT DEFINED.')
170   FORMAT (' IN WPP1CH - ',A4,' CONTROL=',4I4,2X,A4,2X,11I4)
180   FORMAT (' **ERROR** IN WPP1CH - INVALID CONTROL RECORD : ',4I4,2X,
     *   A4,2X,11A4)
190   FORMAT (' IN WPP1CH - IPTRCH=',I5)
200   FORMAT (' IN WPP1CH - MONTH, RECORD:',2I6,' VALUES',I6,2X,F12.6)
210   FORMAT (' IN WPP1CH - IPTRCH=',I5,
     *   ' WORK(JSLOT)=',I6)
220   FORMAT (' **ERROR** IN WPP1CH - READING OR WRITING TO FILE ',I3,
     *     ' ISTAT=',I5)
230   FORMAT (' EXIT WPP1CH - ISTAT=',I3)
C
      END
