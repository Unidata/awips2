C MEMBER PDGNSV
C  (from old member PDPDSTAR)
C-----------------------------------------------------------------------
C
      SUBROUTINE PDGNSV (ILS,IRRBUF,IFRBUF,ISTAT)
C
C          ROUTINE:  PDGNSV
C
C             VERSION:  1.0.0
C
C                DATE:  2-4-83
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C                       8555 16TH ST, SILVER SPRING, MD 587-3700
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE FINDS THE SECOND SMALLEST VALUE IN AN RRS RECORD
C    AND PLACES THE VALUE AND ITS TIME IN THE STATISTICS PORTION
C    OF THE RECORD
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       ILS        I     I     1    LARGE / SMALL FLAG
C                                   1=GET SECOND LARGEST VALUE
C                                   2=GET SECOND SMALLEST VALUE
C       IRRBUF     I    I/O    ?    RRS RECORD
C       IFRBUF     I    I/O    ?    FREE POOL BUFFER
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=OK
C                                     OTHER=SYSTEM ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdbdta'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IRRBUF(1),IFRBUF(1),IARR(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdgnsv.f,v $
     . $',                                                             '
     .$Id: pdgnsv.f,v 1.1 1995/09/17 18:43:57 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA VMISS/-999.0/
C
C***********************************************************************
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDGNSV')
C
      ISTAT=0
C
      CALL USWITC (VMISS,IMISS)
      IARR(1)=IMISS
      IARR(2)=0
      IARR(3)=IMISS
      IARR(4)=0
      ISKIP=IRRBUF(14)
C
      IF (IRRBUF(16).EQ.0) GO TO 90
      IF (IRRBUF(13).EQ.0) GO TO 50
C
C  READ FREE POOL RECORD
      IREC=IRRBUF(13)
20    CALL PDRDFR (IREC,IFRBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
      NUM=IFRBUF(2)
      IPOS=3
      DO 40 I=1,NUM
         IF (IRRBUF(16).LT.JULMIN(ISKIP,IFRBUF(IPOS))) GO TO 90
         IF (IFRBUF(IPOS+1).EQ.IMISS) GO TO 30
            CALL PDCKST (ILS,ISKIP,IARR,IFRBUF(IPOS))
30       IPOS=IPOS+ISKIP
40       CONTINUE
C
C  MORE FREE POOL RECORDS
      IREC=IFRBUF(1)
      IF (IREC.NE.0) GO TO 20
C
C  NOW GO THROUGH REGULAR RRS RECORD
50    NUM=IRRBUF(8)
      IF (NUM.EQ.0) GO TO 90
      IPOS=IRRBUF(9)
      DO 80 I=1,NUM
         IF (IPOS.LE.IRRBUF(1)) GO TO 60
            IPOS=IRRBUF(1)-IRRBUF(7)*IRRBUF(14)+1
60       IF (IRRBUF(16).LT.JULMIN(ISKIP,IRRBUF(IPOS))) GO TO 90
         IF (IRRBUF(IPOS).EQ.IMISS) GO TO 70
            CALL PDCKST (ILS,ISKIP,IARR,IRRBUF(IPOS))
70       IPOS=IPOS+ISKIP
80       CONTINUE
C
C  GET NEW STAT
90    IF (IPDDB.GT.1) WRITE (IOGDB,100) IARR
100   FORMAT (' ARRAY CONTENTS=',2(1X,F7.2,1X,I7))
      GO TO (110,130),ILS
C
C  PUT IN SECOND LARGEST VALUE
110   IF (IARR(1).EQ.IRRBUF(LHDRRS+4).AND.IARR(2).EQ.IRRBUF(LHDRRS+5))
     1    GO TO 120
         IRRBUF(LHDRRS+6)=IARR(1)
         IRRBUF(LHDRRS+7)=IARR(2)
         GO TO 150
120   IRRBUF(LHDRRS+6)=IARR(3)
      IRRBUF(LHDRRS+7)=IARR(4)
      GO TO 150
C
C  PUT IN SECOND SMALLEST VALUE
 130   IF (IARR(1).EQ.IRRBUF(LHDRRS+8).AND.IARR(2).EQ.IRRBUF(LHDRRS+9))
     1    GO TO 140
         IRRBUF(LHDRRS+10)=IARR(1)
         IRRBUF(LHDRRS+11)=IARR(2)
         GO TO 150
140   IRRBUF(LHDRRS+10)=IARR(3)
      IRRBUF(LHDRRS+11)=IARR(4)
C
150   IF (IPDTR.GT.1) WRITE (IOGDB,160)
160   FORMAT (' *** EXIT PDGNSV')
C
      RETURN
C
      END
