C MEMBER HGFNUM
C  (from old member HCLDFUNC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.13:55:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HGFNUM (IGL,IFNUM,ISTAT)
C
C          ROUTINE:  HGFNUM
C
C             VERSION:  1.0.0
C
C                DATE:  2-1-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C  CHANGED 2/7/86 TO PROPERLY SHIFT DELETED FUNCTION NUMBERS
C                 GFS - HRL
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE RETURNS A FUNCTION NUMBER FOR DEFINING FUNCTIONS
C    IT FIRST CHECKS FOR A NUMBER OF A DELETED FUNCTION
C    IF NONE ARE AVAILABLE - IT ASSIGNS THE NEXT AVAILABLE NUMBER
C    ALL POINTER RECORDS ARE UPDATED AND WRITTEN
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I     1    GLOBAL(-) / LOCAL(+) INDICATOR
C
C       IFNUM      I     O     1    FUNCTION NUMBER
C
C       ISTAT      I     O     1    STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=NO AVAILABLE #
C                                      2=DAIO OR SYSTEM ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hcntrl'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hwords'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER ICNTL(4),IXBUF(4),IXBUF2(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hgfnum.f,v $
     . $',                                                             '
     .$Id: hgfnum.f,v 1.1 1995/09/17 18:42:24 dws Exp $
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
      IFNUM=0
      ISTAT=0
C
C        READ IN THE CONTROLS
C
      IXREC=HINDEX(4,8)
      IXUNIT=KINDXG
      CALL UREADT (IXUNIT,IXREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
C
C       SET SEARCH RANGES AND SEE IF ANY NUMBERS SAVED
C
      IF (ICNTL(2).EQ.0) GO TO 100
      IF (IGL.GT.0) GO TO 10
C GLOBAL
      IS=1
      IE=500
      IF (ICNTL(3).EQ.0) GO TO 100
      GO TO 20
10    CONTINUE
C LOCAL
      IS=501
      IE=1000
      IF (ICNTL(4).EQ.0) GO TO 100
20    CONTINUE
C
C        FIND A NUMBER
C
      IREC=IXREC+1
      IEND=IXREC+250
      DO 40 I=IREC,IEND
          CALL UREADT (IXUNIT,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 150
          DO 30 K=1,4
              IF (IXBUF(K).EQ.0) GO TO 150
              IF (IXBUF(K).GE.IS.AND.IXBUF(K).LE.IE) GO TO 50
30            CONTINUE
40        CONTINUE
C
C      FELL THROUGH - ERROR
C
      GO TO 150
50    CONTINUE
C
C          FOUND A FREE NUMBER - GET LAST RECORD USED
C          AND PUT IN PLACE OF FOUND NUMBER
C
      IREC=(ICNTL(2)+3) / 4+IXREC
      IPOS=MOD(ICNTL(2),4)
      IF (IPOS.EQ.0)IPOS=4
C
C         MOVE LAST NUMBER INTO SLOT AND UPDATE POINTERS
C
      IF (IREC.NE.I) GO TO 60
C
C        SAME RECORD - SET UP IXBUF
C
      IFNUM=IXBUF(K)
      IXBUF(K)=IXBUF(IPOS)
      IXBUF(IPOS)=0
      GO TO 70
60    CONTINUE
C
C        DIFFERENT RECORD NUMBERS
C
      CALL UREADT (IXUNIT,IREC,IXBUF2,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
      ITFNUM=IXBUF2(IPOS)
      IXBUF2(IPOS)=0
      IFNUM=IXBUF(K)
      IXBUF(K)=ITFNUM
70    CONTINUE
      IF (IGL.GT.0) ICNTL(4)=ICNTL(4) - 1
      IF (IGL.LT.0) ICNTL(3)=ICNTL(3) - 1
      ICNTL(2)=ICNTL(2) - 1
C
C         WRITE THE RECORDS
C
      IF (IREC.EQ.I) GO TO 80
      CALL UWRITT (IXUNIT,IREC,IXBUF2,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
80    CALL UWRITT (IXUNIT,I,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
      CALL UWRITT (IXUNIT,IXREC,ICNTL,ISTAT)
      IF (ISTAT.NE.0) GO TO 150
      IF (IHCLDB.GT.1) WRITE (IOGDB,90) IFNUM
90    FORMAT (' HGFNUM RETURNED OLD FUNCTION NUMBER ',I3)
      GO TO 170
100   CONTINUE
C
C        HAVE TO GET NEW NUMBER - SET TYPE
C
      IX=10
      MAX=500
      IF (IGL.LT.0) GO TO 110
      IX=11
      MAX=1000
110   CONTINUE
C
C        GET THE NUMBER
C
      IFNUM=HCNTL(IX,2)+1
      IF (IFNUM.LE.MAX) GO TO 130
      IF (IGL.LT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,120) 'GLOBAL'
         ENDIF
      IF (IGL.GT.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,120) 'LOCAL'
         ENDIF
120   FORMAT ('0**ERROR** MAXIMUM ',A,' FUNCTIONS ALREADY DEFINED.')
      ISTAT=1
      GO TO 170
C
130   HCNTL(IX,2)=IFNUM
      IF (IHCLDB.GT.1) WRITE (IOGDB,140) IFNUM
140   FORMAT (' HGFNUM RETURNED NEW FUNCTION NUMBER ',I3)
      GO TO 170
C
150   CALL ULINE (LP,2)
      WRITE (LP,160)
160   FORMAT ('0**ERROR** SYSTEM ERROR IN HGFNUM')
      ISTAT=2
170   CONTINUE
C
      RETURN
C
      END
