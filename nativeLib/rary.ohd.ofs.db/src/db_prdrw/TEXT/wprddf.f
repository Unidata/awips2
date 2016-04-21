C MODULE WPRDDF
C-----------------------------------------------------------------------
C
      SUBROUTINE WPRDDF (ITSID,ITYPE,JHOUR,ITSTEP,NUM,IUNITS,NVALS,
     *   LTSDAT,TSDAT,LIWORK,IWORK,IREC,ISTAT)
C
C  THIS ROUTINE WRITES TIME SERIES DATA TO FUTURE TIME SERIES
C  RECORDS.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM    DESCRIPTION
C       ----     ----   ---   ---    -----------
C       ITSID     A8     I     2     TIME SERIES ID
C       ITYPE     A4     I     1     REGULAR DATA TYPE
C       JHOUR     I      I     1     FIRST HOUR OF DATA
C       ITSTEP    I      I     1     TIME STEP
C       NUM       I      I     1     NUMBER OF TIME STEPS OF DATA
C       IUNITS    A4     I     1     UNITS DATA IS IN
C       NVALS     I      I     1     NUMBER OF DATA WORDS
C       TSDAT     I      I   LTSDAT  TIME SERIES DATA ARRAY
C       LIWORK    I      I     1     DIMENSION OF IWORK
C       IWORK     I     I/O  LIWORK  WORK ARRAY
C       IREC      I     I/O    1     INPUT=RECORD NUMBER OF TIME SERIES
C                                         IF KNOWN, ELSE=0
C                                    OUTPUT=RECORD NUMBER OF NEXT TIME
C                                          SERIES OF SAME DATA TYPE
C       ISTAT     I      O     1     STATUS INDICATOR:
C                                      (SEE WPRDD FOR CODES)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
C
      DIMENSION ITSID(2),TSDAT(LTSDAT),IWORK(LIWORK)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/wprddf.f,v $
     . $',                                                             '
     .$Id: wprddf.f,v 1.2 1999/01/20 15:59:30 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,40)
C
C  CHECK DATA TYPE CODE
      CALL PFDTYP (ITYPE,INDX)
      IF (INDX.NE.0) GO TO 20
C      
10    IF (IPRDB.GT.0) WRITE (LP,50) ITYPE
      ISTAT=1
      GO TO 30
C
20    INDXF=DATFIL(7,INDX)
      IF (INDXF.LE.0) GO TO 10
      ITYP=DATFIL(1,INDXF)
      IFPTR=JHOUR
      ICALL=0
      IF (IPRTR.GT.0) WRITE (IOGDB,60)
      CALL WPRDD (ITSID,ITYP,JHOUR,ITSTEP,NUM,IUNITS,NVALS,
     *   LTSDAT,TSDAT,IFPTR,ICALL,LIWORK,IWORK,IREC,ISTAT)
C
30    IF (IPRDB.GT.0) WRITE (IOGDB,70) ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER WPRDDF')
50    FORMAT ('0**ERROR** IN WPRDDF - ',A4,' IS NOT A VALID DATA TYPE.')
60    FORMAT (' WPRDDF CALLING WPRD ')
70    FORMAT (' *** EXIT WPRDDF : ISTAT=',I2)
C
      END
