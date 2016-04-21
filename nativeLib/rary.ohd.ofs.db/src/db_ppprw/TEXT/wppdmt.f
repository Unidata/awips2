C MEMBER WPPDMT
C  (from old member PPWPPDMT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/21/94.10:49:33 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPPDMT (IPTRMT,ISTAT)
C
C          ROUTINE:  WPPDMT
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
C    THIS ROUTINE WILL DELETE THE MEAN MONTHLY TEMPS FOR 1 STATION
C    FROM THE PREPROCESSOR PARAMETRIC DATA BASE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IPTRMT      I    I     1    SUBSCRIPT IN MMMT RECORD FOR THIS
C                                       STATION
C       ISTAT       I    O     1    STATUS, 0=OK, 1=SYSTEM ERROR
C                                     2=MMMT ALREADY UNUSED
C                                     3=INVALID IPTRMT
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
      INTEGER*2 WORK(32)
      INTEGER*2 NOMMMT
C
      DIMENSION ICNTL(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/wppdmt.f,v $
     . $',                                                             '
     .$Id: wppdmt.f,v 1.1 1995/09/17 18:45:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA NOMMMT/-9999/
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,70)
C
      ISTAT=0
C
      LREC2=LRECPP*2
C
C  GET MEAN MONTHLY TEMPS CONTROL RECORD
      IDXDAT=IPCKDT('MMMT')
      ICREC=IPDTDR(3,IDXDAT)
C
C  CHECK THAT MMMTS IS VALID IN THIS SYSTEM
      IF (ICREC.NE.0) GO TO 10
      IF (IPPDB.GT.0) WRITE (LPE,80)
      ISTAT=1
      GO TO 60
C
C  READ CONTROL RECORD
10    LUFILE=KPPRMU(IPDTDR(2,IDXDAT))
      CALL UREADT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
      IF (IPPDB.GT.0) WRITE (IOGDB,90) ICNTL
C
C  COMPUTE THE RECORD AND SLOT NUMBER
      IF (IPTRMT.LE.ICNTL(9)) GO TO 20
      ISTAT=3
      GO TO 60
C
20    IREC=IUNRCD(IPTRMT,LREC2)+ICNTL(2)-1
      JSLOT=IPTRMT-(IREC-ICNTL(2))*LREC2
      IF (IPPDB.GT.0) WRITE (IOGDB,100) IREC,JSLOT
C
C  GET THE RECORD CONTAINING THIS SLOT
      DO 30 IMO=1,12
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 50
C     REPLACE VALUE WITH NOMMMT, IF ALREADY NOMMMT, ERROR
         IF (WORK(JSLOT).EQ.NOMMMT) GO TO 40
         IF (IPPDB.GT.0) WRITE (IOGDB,110) IMO,WORK(JSLOT)
         WORK(JSLOT)=NOMMMT
         CALL UWRITT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 50
         IREC=IREC+ICNTL(3)
         CALL UREADT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 50
C     REPLACE VALUE WITH NOMMMT, IF ALREADY NOMMMT, ERROR
         IF (WORK(JSLOT).EQ.NOMMMT) GO TO 40
         IF (IPPDB.GT.0) WRITE (IOGDB,110) IMO,WORK(JSLOT)
         WORK(JSLOT)=NOMMMT
         CALL UWRITT (LUFILE,IREC,WORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 50
         IREC=IREC+ICNTL(3)
30       CONTINUE
C
C  CHANGE NUMBER OF STATIONS
      ICNTL(7)=ICNTL(7)-1
C
C  UPDATE CONTROL RECORD
      CALL UWRITT (LUFILE,ICREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      GO TO 60
C
C VALUE ALREADY RESET
40    ISTAT=2
      GO TO 60
C
50    IF (IPPDB.GT.0) WRITE (LPE,120) LUFILE,ISTAT
C
60    IF (IPPTR.GT.0) WRITE (IOGDB,130) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' ENTER WPPDMT')
80    FORMAT (' **ERROR** IN WPPDMT - MEAN MONTHLY TEMPS NOT DEFINED.')
90    FORMAT (' IN WPPDMT - MMMT CONTROL=',4I4,2X,A4,2X,11I4)
100   FORMAT (' IN WPPDMT - IREC=',I6,' JSLOT=',I6)
110   FORMAT (' IN WPPDMT - IMO=',I3,' WORK(JSLOT)=',I5)
120   FORMAT (' **ERROR** IN WPPDMT - READING OR WRITING FROM FILE ',I4,
     *     ' ISTAT=',I5)
130   FORMAT (' EXIT WPPDMT - ISTAT=',I3)
C
      END
