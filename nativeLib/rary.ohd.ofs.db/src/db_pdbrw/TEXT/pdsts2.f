C MEMBER PDSTS2
C  (formerly known as PSECLG)
C  (from old member PDBSSTAT)
C-----------------------------------------------------------------------
C
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDSTS2 (IDATE,LAT,LON,IZ,I2LG,LAT2,LON2,IDAY2,ISTAT)
C
C          ROUTINE:  PDSTS2
C
C             VERSION:  1.0.0
C
C                DATE:  2-28-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SEARCHES THE STRANGER STATION RECORDS LOOKING FOR
C    THE SECOND LARGEST VALUE, AS COMPARED TO THE LARGEST VALUE(INPUT).
C    IT WILL SEARCH THROUGH THE ENTIRE EXISTING FILE UNTIL THE
C    SECOND LARGEST IS FOUND. THE STATISTICS FOR THAT VALUE ARE
C    REPLACED AND PASSED BACK TO ROUTINE PDSTSS, WHICH UPDATES THE
C    RECORD TO THE DAILY DATA FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IDATE     I*4    I     1    DATE OF LARGEST VALUE
C       LAT       I*2    I     2    LATITUDE OF LARGEST VALUE
C       LON       I*2    I     2    LONGITUDE OF LARGEST VALUE
C       IZ         I     I     1    SUBSCRIPT OF PSSR IN DIRECTORY
C       I2LG      I*2    O     1    SECOND LARGEST VALUE FOUND
C       LAT2      I*2    O     1    LATITUDE OF SECOND LARGEST
C       LON2      I*2    O     1    LONGITUDE OF SECOND LARGEST
C       IDAY2      I           1    DATE OF SECOND LARGEST VALUE
C       ISTAT      I     O     1    STATUS INDICATOR
C                                     0=NORMAL RETURN
C                                     4=ERROR READING FILE
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 IBUFF(32),LAT,LON,I2LG,LATSAV,LONSAV,LAT2,LON2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdsts2.f,v $
     . $',                                                             '
     .$Id: pdsts2.f,v 1.3 1997/06/25 13:17:12 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER PDSTS2')
C
      ISTAT=0
C
      LRCP=LRCPDD*2
C
      MISNG=-9999     
      IDAY2=MISNG
      I2LG=MISNG
      LAT2=MISNG
      LON2=MISNG
C
C  GET LOGICAL UNIT NUMBER
      IDF=IDDTDR(4,IZ)
C
C  GET DATE OF FIRST AND LAST DAYS OF DATA ON FILE
      CALL UMEMOV (IDDTDR(8,IZ),IEDATE,1)
      CALL UMEMOV (IDDTDR(11,IZ),LDATE,1)
      NUMDAY=IEDATE
C
C  GET RECORD OF EARLIEST DATE
20    CALL PDCREC (NUMDAY,IEDATE,IZ,IREC)
C
C  READ RECORD
      CALL UREADT (KPDDDF(IDF),IREC,IBUFF,ISTAT)
      IF (ISTAT.NE.0) GO TO 120
C
C  CHECK NUMBER OF VALUES FOR FIRST DAY
      IF (IBUFF(1).LT.0) GO TO 90
      NUMOV=IBUFF(1)
C
C  SEARCH FOR 2ND LARGEST
      LATSAV=IBUFF(2)
      LONSAV=IBUFF(3)
      IF (IPDDB.GT.0) WRITE (IOGDB,*) 'NUMOV=',NUMOV
      J=4
      DO 80 K=1,NUMOV
         IF (IBUFF(J).LE.I2LG) GO TO 50
C     CHECK DATE TO SEE IF SAME REPORT AS LARGEST
         IF (IDATE.NE.NUMDAY) GO TO 40
C     CHECK LAT/LON 
         IF (LAT.EQ.LATSAV.AND.LON.EQ.LONSAV) GO TO 80
C     SAVE THE VALUE AND DAY
40       I2LG=IBUFF(J)
         IDAY2=NUMDAY
         LAT2=LATSAV
         LON2=LONSAV
50       IF (K.EQ.NUMOV) GO TO 80
         IF (J.LT.LRCP) GO TO 60
            IREC=IREC+1
            CALL UREADT (KPDDDF(IDF),IREC,IBUFF,ISTAT)
            IF (ISTAT.NE.0) GO TO 120
            J=0
60       J=J+1
         LATSAV=IBUFF(J)
         IF (J.LT.LRCP) GO TO 70
            IREC=IREC+1
            CALL UREADT (KPDDDF(IDF),IREC,IBUFF,ISTAT)
            IF (ISTAT.NE.0) GO TO 120
            J=0
70       J=J+1
         LONSAV=IBUFF(J)
         J=J+1
         IF (J.LE.LRCP) GO TO 80
            IREC=IREC+1
            CALL UREADT (KPDDDF(IDF),IREC,IBUFF,ISTAT)
            IF (ISTAT.NE.0) GO TO 120
            J=1
80       IF (NUMDAY.EQ.LDATE) GO TO 100
         NUMDAY=NUMDAY+1
         GO TO 20
90       CONTINUE
      GO TO 100
C
120   ISTAT=4
C
100   IF (IPDTR.GT.0) WRITE (IOGDB,110) ISTAT
110   FORMAT (' *** EXIT PDSTS2 - ISTAT=',I2)
C
      RETURN
C
      END
