C MEMBER HDNWLD
C  (from old member HCLDLLDR)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDNWLD (NAME,ITYPE,LDRARR,NUMLDR,IFLAG,ISTAT)
C
C          ROUTINE:  HDNWLD
C
C             VERSION:  1.0.0
C
C                DATE:  3-18-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE DELETES AND WRITES LOCAL DEFINITION REFERENCE
C    RECORDS.IT WRITES THE RECORD IN THE SAME PLACE IF THERE IS ROOM,
C    OTHERWISE IT DELETES THE OLD ONE AND WRITES A NEW ONE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       NAME       A8    I     2    NAME OF DEFINITION
C
C       ITYPE      I     I     1    TYPE OF RECORD
C
C       LDRARR     I     I     ?    ARRAY CONTAINIG POINTERS
C
C       NUMLDR     I     I     1    NUMBER OF POINTERS IN LDRARR
C
C       IFLAG      I     I     1    DELETION ONLY FLAG
C                                   0=DELETE AND WRITE
C                                   1=DELETE ONLY
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                   0=OK   1= ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER LDRARR(1),NAME(2),KBUF(50),IDEL(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdnwld.f,v $
     . $',                                                             '
     .$Id: hdnwld.f,v 1.1 1995/09/17 18:42:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA MAX/50/
      DATA IDEL/4HDELE,4HTED /
C
C***********************************************************************
C
C
      ISTAT=0
C
C        FIND THE RECORD
C
      CALL HSRLDR (NAME,ITYPE,1,IREC,ISTAT)
      IF (IREC.NE.0) GO TO 20
      WRITE (LPE,10) NAME
10    FORMAT (' **ERROR** LOCAL DEFINITION REFERENCE RECORD FOR ',
     1       2A4,' NOT FOUND')
      ISTAT=1
      GO TO 90
20    CONTINUE
      CALL HRLDRR (IREC,KBUF,MAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
C
C        SEE IF ROOM IN OLD RECORD
C
      IF (IFLAG.EQ.1) GO TO 40
      IF ((NUMLDR+10)/4.GT.KBUF(1)) GO TO 40
      CALL UMEMOV (LDRARR,KBUF(8),NUMLDR)
      CALL WVLRCD (KINDXG,IREC,KBUF(1),KBUF,4,ISTAT)
      IF (ISTAT.NE.0) GO TO 70
      IF (IHCLDB.EQ.3) WRITE (IOGDB,30)
30    FORMAT (' WROTE LDR IN SAME PLACE')
      GO TO 90
40    CONTINUE
C
C        DELETE THE OLD LDR
C
      CALL UMEMOV (IDEL,KBUF(3),2)
      KBUF(2)=0
      CALL UWRITT (KINDXG,IREC,KBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 70
      IF (IHCLDB.EQ.3) WRITE (IOGDB,50)
50    FORMAT (' DELETED THE LDR')
C
C        WRITE A NEW LDR RECORD
C
      IF (IFLAG.EQ.1) GO TO 90
      ITYP=IABS(ITYPE)
      CALL HPUTLD (ITYP,NAME,NUMLDR,LDRARR,ISTAT)
      IF (ISTAT.NE.0) GO TO 90
      IF (IHCLDB.EQ.3) WRITE (IOGDB,60)
60    FORMAT (' WROTE A NEW LDR RECORD')
      GO TO 90
70    CONTINUE
C
C         SYSTEM ERROR
C
      WRITE (LPE,80)
80    FORMAT (' **ERROR** SYSTEM ERROR IN HDNWLD')
      ISTAT=2
      GO TO 90
C
90    CONTINUE
C
      RETURN
C
      END
