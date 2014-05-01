C MEMBER URFPRD
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 01/12/94.10:40:51 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE URFPRD (IDSTA,IREC,NFRECN,NFREC1,ISTAT)
C
C          ROUTINE:  URFPRD
C
C             VERSION:  1.0.0
C
C                DATE:  5-21-85
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE READS A FREEPOOL RECORD FROM THE OLD DATA FILE,
C    CHECKS FOR FREE POOL RECORDS AND WRITES IT TO THE NEW FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IREC       I    I     1     RECORD NUMBER OF FREEPOOL TO READ
C       NFRECN     I    I/O   1     RECORD NUMBER OF NEXT AVAILABLE
C                                   FREEPOOL RECORD
C       NFREC1     I    O     1     POINTER OF FIRST FREEPOOL WRITTEN
C       ISTAT      I    O     1     STATUS CODE
C                                    0=NORMAL RETURN
C                                    OTHER=ERROR
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/urunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IDSTA(2),IFRBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urfprd.f,v $
     . $',                                                             '
     .$Id: urfprd.f,v 1.1 1995/09/17 19:17:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,90)
C
      ISTAT=0
C
      NFREC1=NFRECN
C
      IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,100) NFRECN
C
C  READ FREEPOOL RECORD FROM OLD FILE
10    CALL PDRDFR (IREC,IFRBUF,ISTAT)
      IXREC=IREC
      IF (ISTAT.NE.0) GO TO 60
C
C  SEE IF ANOTHER FREEPOOL EXISTS
      IF (IFRBUF(1).GE.0) GO TO 20
         WRITE (LP,110) IFRBUF(1),IDSTA
         CALL SUWRNS (LP,2,-1)
         GO TO 75
C
C  WRITE FREEPOOL TO NEW FILE
20    IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,120) IFRBUF(1)
      ISAVE=IFRBUF(1)
      IF (ISAVE.EQ.0) GO TO 30
         IFRBUF(1)=NFRECN+1
         IF (NFRECN.GT.MAXFRC) GO TO 40
30    CALL UWRITT (KURDDF(IUFREE),NFRECN,IFRBUF,ISTAT)
      IXREC=NFRECN
      IF (ISTAT.NE.0) GO TO 60
C
C  UPDATE RECORD NUMBER OF NEXT AVAILABLE FREEPOOL RECORD
      NFRECN=NFRECN+1
      IF (IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPDDB.GT.0) WRITE (IOGDB,130) NFRECN
      IREC=ISAVE
      IF (IREC.NE.0) GO TO 10
      GO TO 80
C
40    CONTINUE
      WRITE (LP,50) NFRECN
      CALL SUERRS (LP,2,-1)
50    FORMAT ('0*** ERROR - IN URFPRD - NOT ENOUGH FREEPOOL SPACE TO ',
     *   'WRITE RECORD ',I6,' TO NEW FILES.')
      GO TO 80
C
60    CONTINUE
      WRITE (LP,70) IXREC
      CALL SUERRS (LP,2,-1)
70    FORMAT ('0*** ERROR - IN URFPRD - TRYING TO READ OR WRITE ',
     *   'FREEPOOL RECORD ',I5,'.')
C
75    IWURFL=1
C
80    IF (IPPDB.GT.0.OR.IPDDB.GT.0) CALL SULINE (IOGDB,1)
      IF (IPPDB.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,140)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' *** ENTER URFPRD')
100   FORMAT (' NFRECN=',I6)
110   FORMAT ('0*** WARNING - IN URFPRD - INVALID FREEPOOL RECORD ',
     *   'POINTER (',I6,') ENCOUNTERED FOR STATION ',2A4,'.')
120   FORMAT (' IFRBUF(1)=',I6)
130   FORMAT (' NFRECN=',I6)
140   FORMAT (' *** EXIT URFPRD')
C
      END
