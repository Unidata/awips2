C MODULE HSRLDR
C-----------------------------------------------------------------------
C
      SUBROUTINE HSRLDR  (NAME,ITYPE,IOPER,IREC,ISTAT)
C
C          ROUTINE:  HSRLDR
C             VERSION:  1.0.0
C                DATE:  3-18-82
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SEARCHES THE LOCAL DEFINITION REFERENCE RECORDS
C    AND WRITES OUT ERROR MESSAGES WHEN A MATCH IS FOUND.
C       IOPER=1 - FINDS NAME AND TYPE MATCH ANDS RETURNS RECORD NUMBER
C       IOPER=2 - FINDS ALL NAME AND TYPE MATCHES
C       IOPER=3 - FINDS ALL POINTER MATCHES (POINTER IS ITYPE)
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME       A8    I     1    NAME OF DEFINITION TO SEARCH FOR
C       ITYPE      I     I     1    TYPE OF DEFINITION
C                                   INTERNAL NUMBER IF IOPER=3
C       IOPER      I     I     1    TYPE OF SEARCH 1-3
C       IREC       I     O     1    RECORD # WHERE FOUND IF IOPER=1
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     1=MATCH FOUND
C                                     2=SYSTEM ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hdflts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      PARAMETER (LKBUF=100)
      INTEGER KBUF(LKBUF),IDEL(2),NAME(2),LTYP(3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hsrldr.f,v $
     . $',                                                             '
     .$Id: hsrldr.f,v 1.2 1998/04/07 13:02:36 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IDEL/4HDELE,4HTED /
      DATA LTYP/4HPROC,4HFUNC,4HTECH/
C
C***********************************************************************
C
C
      IREC=0
      ISTAT=0
C
C  GET FIRST AND LAST RECORD NUMBER
      ISTRT=HINDEX(2,8)
      IEND=HINDEX(3,8)+1
      IF (IEND.EQ.ISTRT) GO TO 80
C
C  READ THE RECORDS
10    CALL HRLDRR (ISTRT,KBUF,LKBUF,ISTA)
      IF (ISTA.NE.0) THEN
         WRITE (LP,110)
110   FORMAT ('0**ERROR** IN HSRLDR - ERROR ENCOUNTERED CALLING ',
     *   'ROUTINE HRLDRR.')
         GO TO 120
         ENDIF
      LTYPE=LTYP(KBUF(2))
C
C  CHECK IF DELETED
      CALL UNAMCP (IDEL,KBUF(3),ISTA)
      IF (ISTA.EQ.0) GO TO 70
      IF (IOPER.EQ.3) GO TO 40
C
C  CHECK IF NAME AND TYPE ARE SAME
      CALL UNAMCP (NAME,KBUF(3),ISTA)
      IF (ISTA.NE.0) GO TO 70
      IF (IABS(ITYPE).NE.KBUF(2)) GO TO 70
      IF (IOPER.EQ.1) GO TO 30
C
C  FOUND A LOCAL WITH SAME NAME
      WRITE (LP,20) NAME,LTYPE,KBUF(5),KBUF(6)
20    FORMAT ('0**ERROR**  DEFINITION ',2A4,' PREVIOUSLY DEFINED AS ',
     *   'A LOCAL ',A4,' BY USER ',2A4)
      ISTAT=1
      GO TO 70
30    CONTINUE
C
C  FOUND THE RECORD FOR A DELETE - CHECK USER
      CALL UNAMCP (KBUF(5),HNAMRF,ISTA)
      IF (ISTA.NE.0) GO TO 70
      IREC=ISTRT
      GO TO 120
C
C  SEARCH FOR POINTER IN RECORD
40    IF (KBUF(7).EQ.0) GO TO 70
      NUM=KBUF(7)+7
      DO 60 I=8,NUM
         IF (KBUF(I).NE.ITYPE) GO TO 60
         WRITE (LP,50) NAME,LTYPE,(KBUF(J),J=3,6)
50       FORMAT ('0**ERROR** ',2A4,' FOUND IN LOCAL ',A4,
     *        ' DEFINITION RECORD ',2A4,' FOR USER ',2A4,'.')
         ISTAT=1
60       CONTINUE
C
C  CHECK IF MORE TO READ
70    ISTRT=ISTRT+KBUF(1)
      IF (ISTRT.LT.IEND) GO TO 10
C
80    IF (IHCLDB.GT.2.AND.ISTAT.EQ.0) WRITE (IOGDB,90)
90    FORMAT (' NO REFERENCES FOUND IN HSRLDR')
C
120   RETURN
C
      END
