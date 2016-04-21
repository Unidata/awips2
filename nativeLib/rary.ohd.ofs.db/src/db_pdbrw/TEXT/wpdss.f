C MEMBER WPDSS
C  (from old member PDWPDSS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/28/95.16:08:14 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE WPDSS (JDAY,IENDHR,RLATLN,VAL,IUNIT,IREV,ISTAT)
C
C          ROUTINE:  WPDSS
C
C             VERSION:  1.0.1
C
C                DATE:  2-15-83
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WRITES STRANGER PRECIPITATION DATA FOR ONE
C    STRANGER REPORT ON THE PREPROCESSOR DATA BASE. THE INPUT
C    LATITUDE AND LONGITUDE ARE CONVERTED TO POLAR STEREOGRAPHIC
C    COORDINATES; AN UPDATE OCCURS WHEN A MATCH IS FOUND ON THE RECORD.
C    THE PRECIPITATION VALUE INTO THE PROPER REPORTING UNITS.
C    IF NO REPORTED MATCH IS FOUND AND THE DATE IS WITHIN THE
C    RETENTION RANGE THEN A NEW DAYS DATA IS CREATED ON THE FILE.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       JDAY      I      I     1     JULIAN DAY OF STRANGER REPORT
C       IENDHR    I      I     1     LAST HR OF STRNGER REPRT FOR DAY
C       RLATLN    R*4    I     2     LAT/LON OF REPORT(DECIMAL DEGREE)
C       VAL       R*4    I     1     PRECIPITATION VALUE
C       IUNIT     A*4    I     1     UNITS OF PRECIP VALUE (INCHES)
C       IREV      I      I     1     REVISION FLAG
C                                       O=A NON-REVISION WRITE
C                                       1=A REVISION WRITE
C       ISTAT     I      O     1     STATUS INDICATOR
C                                      0=OK,NORMAL RETURN
C                                      1=DATE NOT CONTINUOUS WITH
C                                          OTHERS(VALUE NOT WRITTEN)
C                                      2=DATA NOT DEFINED ON PPDB
C                                      3=INVALID UNITS CONVERSION
C                                      4=SYSTEM ERROR ACCESSING FILE
C                                      20+=INVALID REVISION FLAG
C                                      30+=INVALID VALUE
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
      DIMENSION RLATLN(2),ISTAID(2)
      INTEGER*2 ISSBUF(32),IX,IY,IV
      INTEGER*2 MAXRPT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdss.f,v $
     . $',                                                             '
     .$Id: wpdss.f,v 1.3 1996/07/12 18:00:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA ISTAID/4HSTRA,4HNGER/
C
C***********************************************************************
C
C
C
      LRCP=LRCPDD*2
      IWRFLG=0
      ISTAT=0
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER WPDSS')
C
C  FIND TYPE IN DIRECTORY
      IZ=IPDCKD('PPSR')
C
      IF (IPDDB.GT.0) WRITE (IOGDB,20) (IDDTDR(II,IZ),II=1,24)
20    FORMAT (' DATA DIRECTORY:' /
     *    1X,12(I5,1X) / 1X,12(I5,1X))
      IDF=IDDTDR(4,IZ)
      IF (IDDTDR(7,IZ).LT.1) GO TO 190
C
C  CHECK IF ANY DATA TYPES HAVE DATA
      DO 30 I=1,NMDTYP
         IF (IDDTDR(8,I).NE.0.OR.IDDTDR(9,I).NE.0) GO TO 40
30       CONTINUE
C
C  INITIALIZE FIRST DAY
      CALL PDDAY1 (JDAY)
C
40    CALL UMEMOV (IDDTDR(8,IZ),IEDATE,1)
      CALL UMEMOV (IDDTDR(11,IZ),LDATE,1)
      IF (JDAY.GE.IEDATE.AND.JDAY.LE.LDATE+1) GO TO 50
         ISTAT=1
         GO TO 250
C
50    IF (JDAY.GE.IEDATE.AND.JDAY.LE.LDATE) GO TO 60
C
C  CREATE A NEW DAY OF DATA
      CALL PDNEWD (JDAY,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
C  RESET EARLIEST DAY
      CALL UMEMOV (IDDTDR(8,IZ),IEDATE,1)
C
C  FIND WHERE THIS RECORD BEGINS
60    CALL UMEMOV (IDDTDR(16,IZ),MAXRPT,1)
      IF (MAXRPT.EQ.0) GO TO 190
C
C  READ RECORD
      CALL PDCREC (JDAY,IEDATE,IZ,IREC)
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' JDAY=',JDAY,
     *   ' IREC=',IREC,
     *   ' '
C
C  READ RECORD INTO BUFFER
      CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      NREC=IREC
C
C  CHECK OFFSET
      IF (ISSBUF(1).LT.0) ISSBUF(1)=0
      NUMRPT=ISSBUF(1)
C
C  CHECK IF NUMBER OF REPORTS EXCEEDS MAX FOR DAY
      IF (NUMRPT.GT.MAXRPT) NUMRPT=MAXRPT
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' MAXRPT=',MAXRPT,
     *   ' NUMRPT=',NUMRPT,
     *   ' '
C
C  CONVERT LAT/LON
      CALL SBLLGD (RLATLN(2),RLATLN(1),1,X,Y,1,ISTAT)
      IX=(X*10)+.5
      IY=(Y*10)+.5
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   'IX=',IX,
     *   'IY=',IY,
     *   ' '
C
C  CHECK FOR LAT/LON MATCH
      J=2
      IF (NUMRPT.EQ.0) GO TO 120
      DO 110 I=1,NUMRPT
         IF (IY.NE.ISSBUF(J)) GO TO 90
         IF (J.LT.LRCP) GO TO 70
            IREC=IREC+1
            CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
            IF (ISTAT.NE.0) GO TO 240
            J=0
C     FOUND THE LATITUDE - NOW CHECK FOR LONGITUDE
70       J=J+1
         IF (IX.NE.ISSBUF(J)) GO TO 100
         J=J+1
         IF (J.LT.LRCP) GO TO 80
         IREC=IREC+1
         CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         J=1
C     FOUND BOTH MATCHES
80       IF (IPDDB.GT.0) WRITE (LP,*)
     *     ' J=',J,
     *     ' IY=',IY,
     *     ' IX=',IX,
     *     ' '
         GO TO 150
C     KEEP LOOKING FOR LATITUDE(IY)
90       J=J+3
         IF (J.LE.LRCP) GO TO 110
         IREC=IREC+1
         CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         J=J-LRCP
         GO TO 110
100      J=J+2
         IF (J.LE.LRCP) GO TO 110
C     READ NEXT RECORD
         IREC=IREC+1
         CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
         IF (ISTAT.NE.0) GO TO 240
         J=J-LRCP
110      CONTINUE
C
C  CHECK IF NUMBER OF REPORTS FOR THE DAY EQUALS THE MAXIMUM
      IF (NUMRPT.EQ.MAXRPT) GO TO 210
C
C  STORE LATITUDE IN NEW REPORT
120   ISSBUF(J)=IY
      J=J+1
      IF (J.LE.LRCP) GO TO 130
      CALL UWRITT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IREC=IREC+1
      CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      J=1
C
C  STORE LONGITUDE IN NEW REPORT
130   ISSBUF(J)=IX
      J=J+1
      IF (J.LE.LRCP) GO TO 140
      CALL UWRITT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IREC=IREC+1
      CALL UREADT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      J=1
C
140   IWRFLG=1
      NUMRPT=NUMRPT+1
      IF (IPDDB.GT.0) WRITE (LP,*) 'NUMRPT=',NUMRPT
C
C  CONVERT UNITS AND VALUES BEFORE UPDATING
150   CALL UDUCNV (IUNIT,'IN  ',2,1,FACTOR,TFACT,ISTAT)
      IF (ISTAT.NE.0) GO TO 200
      IDX=1
      CALL PDCVPP (VAL,FACTOR,IREV,IENDHR,ISSBUF(J),IDX,ISTAID,ISTAT)
      IF (ISTAT.NE.0) GO TO 220
      CALL PDGTPP (ISSBUF(J),V,IDX,IVV)
      IV=IVV
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IVV=',IVV,
     *   ' ISSBUF(J)=',ISSBUF(J),
     *   ' '
C
C  CHECK IF STILL IN FIRST RECORD
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' NREC=',NREC,
     *   ' IREC=',IREC,
     *   ' '
      IF (NREC.EQ.IREC) GO TO 160
C
C  WRITE UPDATED RECORD
      CALL UWRITT (KPDDDF(IDF),IREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' IREC=',IREC,
     *   ' '
C
      IF (IWRFLG.NE.1) GO TO 180
C
C  RE-READ FIRST RECORD TO UPDATE STATION COUNTER
      CALL UREADT (KPDDDF(IDF),NREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
C  CHECK IF THIS IS A NEW REPORT
160   IF (IWRFLG.NE.1) GO TO 170
C
C  UPDATE COUNTER AND RE-WRITE FIRST RECORD
      ISSBUF(1)=NUMRPT
170   IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' NREC=',NREC,
     *   ' '
      CALL UWRITT (KPDDDF(IDF),NREC,ISSBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
C
C  UPDATE DIRECTORY RECORDS
180   IF (IDDTDR(17,IZ).LT.NUMRPT) IDDTDR(17,IZ)=NUMRPT
      IF (IDDTDR(19,IZ).LT.(NUMRPT*3)+1) IDDTDR(19,IZ)=(NUMRPT*3)+1
C
C  UPDATE STRANGER STATION STATISTICS RECORD
      CALL PDSTSS (JDAY,IX,IY,IV,IZ,IDF,IWRFLG,ISTAT)
      IF (ISTAT.NE.0) GO TO 240
      GO TO 220
C
C  STRANGER STATIONS NOT DEFINED IN PREPROCESSOR DATA BASE
190   ISTAT=2
      GO TO 220
C
C  INVALID UNITS CONVERSION
200   ISTAT=3
      GO TO 220
C
C  ALREADY HAVE MAX STATIONS FOR DAY
210   ISTAT=5
      GO TO 220
C
220   IF (IPDTR.GT.0) WRITE (IOGDB,230) ISTAT
230   FORMAT (' *** EXIT WPDSS - ISTAT=',I2)
      GO TO 250
C
240   ISTAT=4
C
250   RETURN
C
      END
