C MEMBER PDSET0
C  (from old member PDWPD1S)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 09/28/95.16:01:30 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PDSET0 (DTYPE,IREC1,NREC1D,IFILE,ISTAT)
C
C  THIS ROUTINE INITIALIZES THE DATA RECORDS FOR ONE DAY.
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       DTYPE       I     I     1    DATA TYPE
C       IREC1       I     I     1    FIRST RECORD NUMBER
C       NREC1D      I     I     1    NUMBER OF RECORDS FOR ONE DAY
C       IFILE       I     I     1    FILE SUBSCRIPT
C       ISTAT       I     O     1    STATUS CODE
C                                      0=NORMAL RETURN
C                                      1=ERROR
C
      PARAMETER (LARRAY=32)
C
      INTEGER*2 IARRAY(LARRAY)
C
      CHARACTER*4 DTYPE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdsifc'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdset0.f,v $
     . $',                                                             '
     .$Id: pdset0.f,v 1.2 1996/01/16 22:54:04 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** ENTER PDSET0'
C
      ISTAT=0
C
C  SET VALUE FOR MISSING DATA
      MISSVL=MISSNG
      IF (DTYPE.EQ.'PP24'.OR.DTYPE.EQ.'PPSR') MISSVL=MISSPP
      LRCPD2=LRCPDD*2
C
      IF (IPDDB.GT.0) WRITE (IOGDB,*)
     *   ' DTYPE=',DTYPE,
     *   ' IREC1=',IREC1,
     *   ' NREC1D=',NREC1D,
     *   ' IFILE=',IFILE,
     *   ' LRCPD2=',LRCPD2,
     *   ' LARRAY=',LARRAY,
     *   ' MISSVL=',MISSVL,
     *   ' '
C
      IF (LRCPD2.GT.LARRAY) THEN
         WRITE (LP,30) LRCPD2,LARRAY
         ISTAT=1
         GO TO 20
         ENDIF
C
C  INITIALIZE RECORDS
      CALL UMEMS2 (MISSVL,IARRAY,1,LRCPD2)
      LSTREC=IREC1+NREC1D-1
      DO 10 IREC=IREC1,LSTREC
         CALL UWRITT (KPDDDF(IFILE),IREC,IARRAY,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (LP,40) IREC,KPDDDF(IFILE)
            GO TO 20
            ENDIF
10       CONTINUE
C
      IF (IPDDB.GT.0) WRITE (LP,50) IREC1,LSTREC,DTYPE,KPDDDF(IFILE)
C
20    IF (IPDTR.GT.0) WRITE (IOGDB,*) '*** EXIT PDSET0 -',
     *   ' ISTAT=',ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT ('0**ERROR** IN PDSET0 - NUMBER OF ARRAY WORDS NEEDED (',
     *   I3,') EXCEEDS ARRAY SIZE (',I3,'.')
40    FORMAT ('0**ERROR** IN PDSET0 - WRITING RECORD ',I6,
     *   ' TO UNIT ',I2,'.')
50    FORMAT ('0**NOTE** IN PDSET0 - DATA RECORDS ',I6,' THROUGH ',I6,
     *   ' INITIALIZED FOR DATA TYPE ',A,
     *   ' ON UNIT ',I2,'.')
C
      END
