C MEMBER PPPUTR
C  (from old member PPPPPUTR)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/06/95.08:55:26 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PPPUTR (IDXDAT,IXREC,ID,IARRAY,LARRAY,IWORK,IPTR,
     *   ISTAT)
C
C          SUBROUTINE:  PPPUTR
C
C             VERSION:  1.0.0
C
C                DATE:  11-29-82
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL PUT A PARAMETER RECORD INTO THE PARAMETER
C    FILE AND ALSO MAKE THE ENTRY IN THE INDEX FILE.  THE INDEX
C    RECORD NUMBER IS AN ARGUMENT, SO THAT THIS WILL ALSO DO A
C    REPLACE.  ALL CONTROL VALUES ARE UPDATED IF THE WRITES ARE
C    SUCCESSFUL.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IDXDAT     I     I     1    SUBSCRIPT OF THE PARAMETER DIRECTRY
C       IXREC      I     I     1    INDEX RECORD NUMBER
C       ID         A     I     2    PARAMETER ID
C       IARRAY     I     I  LARRAY  PARAMETER RECORD
C       LARRAY     I     I     1    LENGTH OF IARRAY
C       IWORK      I    I/O LRCLPP  WORK BUFFER
C       IPTR       I     O     1    RECORD NUMBER OF PARAMETER RECORD
C       ISTAT      I     O     1    STATUS CODE
C                                     0=OK
C                                     1=READ OR WRITE ERROR
C                                     2=FILE IS FULL
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'urcommon/urunts'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urcdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION ID(2),IWORK(*),IARRAY(*)
      DIMENSION IXBUF(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_ppprw/RCS/ppputr.f,v $
     . $',                                                             '
     .$Id: ppputr.f,v 1.2 1996/01/16 23:19:08 page Exp $
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
      IF (IPPTR.GT.0) WRITE (IOGDB,120)
C
      ISTAT=0
C
      CALL UMEMST (0,IWORK,LRECPP)
      IWORK(1)=LARRAY+NPPHED
      CALL UMEMOV (ID,IWORK(2),2)
C
C  CHECK IF REORDER PROGRAM
      IF (IAMORD.EQ.1) THEN
         IX=JPDTDR(2,IDXDAT)
         LUPARM=KUPRMI(IX)
         IWORK(4)=JPDTDR(1,IDXDAT)
         GO TO 10
         ENDIF
      IX=IPDTDR(2,IDXDAT)
      LUPARM=KPPRMU(IX)
      IWORK(4)=IPDTDR(1,IDXDAT)
10    N=LRECPP-NPPHED
      IF (N.GT.LARRAY) N=LARRAY
      CALL UMEMOV (IARRAY,IWORK(NPPHED+1),N)
      CALL UMEMOV (ID,IXBUF,2)
C
C  CHECK IF THERE IS ROOM IN FILE
      NREC=IUNRCD(IWORK(1),LRECPP)
      IF (IAMORD.EQ.0.AND.NREC+IPMCTL(2,IX).GT.IPMCTL(1,IX)) GO TO 100
      IF (IAMORD.EQ.1.AND.NREC+JPMCTL(2,IX).GT.JPMCTL(1,IX)) GO TO 100
C
C  SET UP INDEX RECORD
      IF (IAMORD.EQ.1) THEN
         IXBUF(3)=JPDTDR(1,IDXDAT)
         IPTR=JPMCTL(2,IX)+1
         GO TO 20
         ENDIF
      IXBUF(3)=IPDTDR(1,IDXDAT)
      IPTR=IPMCTL(2,IX)+1
20    IXBUF(4)=IPTR
C
C  MAKE THE ENTRY IN THE PARAMETER FILE
      IF (IPPDB.GT.0) WRITE (IOGDB,130) ID,LUPARM,IPTR,
     *  (IWORK(I),I=1,16)
      CALL UWRITT (LUPARM,IPTR,IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      LEFT=LARRAY-N
      IF (LEFT.LE.0) GO TO 40
C
C  WRITE OUT REST OF PARAMETER RECORD
      IREC1=IPTR+1
      NREC1=NREC-2
C
C  CHECK IF LAST RECORD NEEDS TO BE FILLED WITH ZEROS
      IF (NREC1.LE.0) GO TO 30
      IF (IPPDB.GT.0) WRITE (IOGDB,*)
     *   ' IREC1=',IREC1,
     *   ' NREC1=',NREC1,
     *   ' '
      CALL WVLRCD (LUPARM,IREC1,NREC1,IARRAY(N+1),LRECPP,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C  WRITE THE LAST RECORD AND MAKE SURE ZERO FILLED AT END
30    LEFT=LEFT-(NREC1*LRECPP)
      IF (LEFT.LE.0) GO TO 40
      CALL UMEMST (0,IWORK,LRECPP)
      CALL UMEMOV (IARRAY(LARRAY-LEFT+1),IWORK(1),LEFT)
      IREC1=IREC1+NREC1
      IF (IPPDB.GT.0) WRITE (IOGDB,*)
     *   ' IREC1=',IREC1,
     *   ' LEFT=',LEFT,
     *   ' '
      CALL UWRITT (LUPARM,IREC1,IWORK,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C  UPDATE INDEX
40    IF (IAMORD.EQ.0) CALL UWRITT (KPPIDX,IXREC,IXBUF,ISTAT)
      IF (IAMORD.EQ.1) CALL UWRITT (KURIDX,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C  UPDATE CONTROLS
      IF (IAMORD.EQ.0) GO TO 60
C
C  REORDER PROGRAM
      JPMCTL(2,IX)=JPMCTL(2,IX)+NREC
      JPMCTL(3,IX)=JPMCTL(3,IX)+1
C
C  SET POINTER TO FIRST OF TYPE IF NOT YET SET
      IF (JPDTDR(3,IDXDAT).EQ.0) JPDTDR(3,IDXDAT)=IPTR
C
C  GET LAST OF TYPE AND SET PTR TO THIS ONE
      IF (JPDTDR(4,IDXDAT).EQ.0) GO TO 50
         CALL UREADT (LUPARM,JPDTDR(4,IDXDAT),IWORK,ISTAT)
         IWORK(5)=IPTR
         CALL UWRITT (LUPARM,JPDTDR(4,IDXDAT),IWORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 90
50    JPDTDR(4,IDXDAT)=IPTR
      JPDTDR(5,IDXDAT)=JPDTDR(5,IDXDAT)+1
      GO TO 80
C
C  NOT REORDER PROGRAM
60    IPMCTL(2,IX)=IPMCTL(2,IX)+NREC
      IPMCTL(3,IX)=IPMCTL(3,IX)+1
C
C  SET POINTER TO FIRST OF TYPE IF NOT YET SET
      IF (IPDTDR(3,IDXDAT).EQ.0) IPDTDR(3,IDXDAT)=IPTR
C
C  GET LAST OF TYPE AND SET PTR TO THIS ONE
      IF (IPDTDR(4,IDXDAT).EQ.0) GO TO 70
         CALL UREADT (LUPARM,IPDTDR(4,IDXDAT),IWORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 90
         IWORK(5)=IPTR
         CALL UWRITT (LUPARM,IPDTDR(4,IDXDAT),IWORK,ISTAT)
         IF (ISTAT.NE.0) GO TO 90
70    IPDTDR(4,IDXDAT)=IPTR
      IPDTDR(5,IDXDAT)=IPDTDR(5,IDXDAT)+1
C
80    GO TO 110
C
90    IF (IPPDB.GT.0) WRITE (IOGDB,*)
     *   ' READ OR WRITE ERROR :',
     *   ' LUPARM=',LUPARM,
     *   ' '
      ISTAT=1
      GO TO 110
C
100   IF (IPPDB.GT.0) WRITE (IOGDB,*)
     *   ' FILE IS FULL :',
     *   ' LUPARM=',LUPARM,
     *   ' NREC=',NREC,
     *   ' '
      ISTAT=2
C
110   IF (IPPTR.GT.0) WRITE (IOGDB,140) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' *** ENTER PPPUTR')
130   FORMAT (' ID=',2A4,' LUPARM=',I3,' IPTR=',I6,
     *  ' (IWORK(1...16)=',I5,1X,3(A4,1X),I6,11(A4,1X))
140   FORMAT (' *** EXIT PPPUTR : ISTAT=',I3)
C
      END
