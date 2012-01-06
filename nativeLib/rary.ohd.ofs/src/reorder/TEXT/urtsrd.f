C MODULE URTSRD
C-----------------------------------------------------------------------
C
      SUBROUTINE URTSRD (LWORK1,IWORK1,LWORK2,IWORK2,LWORK3,IWORK3,
     *   ISTAT)
C
C  THIS ROUTINE WILL PROCESS THE OLD DATA/TYPE INDEX FOR THOSE
C  TIME SERIES RECORDS THAT HAVE NOT BEEN COPIED TO THE NEW FILES.
C
C  ARGUMENT LIST:
C        NAME    TYPE  I/O  DIM     DESCRIPTION
C        ------  ----  ---  ------  -----------
C        LWORK1    I    I     1     LENGTH OF ARRAY IWORK1
C        IWORK1    I    I   LWORK1  ARRAY FOR TIME SERIES
C        LWORK2    I    I     1     LENGTH OF ARRAY IWORK2
C        IWORK2    I    I   LWORK2  ARRAY FOR FUTURE TIME SERIES
C        LWORK3    I    I     1     LENGTH OF ARRAY IWORK3
C        IWORK3    I    I   LWORK3  ARRAY FOR RRS PARAMETER RECORD
C        ISTAT     I    O      1    STATUS CODE
C                                     0=NORMAL RETURN
C                                     OTHER=ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urcdta'
C
      CHARACTER*4 ITYPE,ITDTYP
      CHARACTER*8 ITSID
C
      DIMENSION IWORK1(LWORK1),IWORK2(LWORK2),IWORK3(LWORK3)
      DIMENSION ITBUF(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urtsrd.f,v $
     . $',                                                             '
     .$Id: urtsrd.f,v 1.3 1998/07/06 13:21:16 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,80)
         ENDIF
C
      ISTAT=0
C
      NUMTS=0
C
      CALL SULINE (LP,2)
      WRITE (LP,90)
C
C  PROCESS EACH TIME SERIES TYPE
      DO 50 I=1,NUMDTP
C     CHECK IF ANY TIME SERIES DEFINED
         IF (DATFIL(15,I).EQ.0) GO TO 50
C     SEE IF 'FC' TYPE
         IF (DATFIL(11,I).EQ.1) GO TO 50
C     OBTAIN FIRST TIME SERIES OF THIS TYPE FROM FILE
         CALL UMEMOV (DATFIL(1,I),ITYPE,1)
         ITSREC=DATFIL(8,I)
         IUNIT=DATFIL(2,I)
C     GET TIME SERIES RECORD
10       CALL RVLRCD (IUNIT,ITSREC,1,ITBUF,LRECLT,ISTAT)
         IF (ISTAT.NE.0) GO TO 60
C     GET IDENTIFIER FROM HEADER
         CALL UMEMOV (ITBUF(4),ITSID,2)
C     CHECK IF IDENTIFIER IS DELETED
         IF (ITSID.EQ.'DELETED') GO TO 40
C     GET DATA TYPE FROM HEADER
         CALL UMEMOV (ITBUF(6),ITDTYP,1)
         IF (ITDTYP.NE.ITYPE) THEN
            WRITE (LP,100) ITDTYP,ITSREC,ITYPE
            CALL SUERRS (LP,2,-1)
            GO TO 40
            ENDIF
C     COPY TIME SERIES IF IT DOES NOT ALREADY EXISTS IN NEW FILES
         CALL URRDTS (ITSID,ITYPE,LWORK1,IWORK1,LWORK2,IWORK2,
     *      LWORK3,IWORK3,NUMTS,LTSHDR,ISTAT)
         IF (ISTAT.GT.0) IWURFL=1
40       IF (ITBUF(13).EQ.0) GO TO 50
         ITSREC=ITBUF(13)
         GO TO 10
50       CONTINUE
C
      CALL SULINE (LP,2)
      WRITE (LP,110) NUMTS
      GO TO 70
C
60    WRITE (LP,120) ITSREC,IUNIT
      CALL SUERRS (LP,2,-1)
      IWURFL=ISTAT
C
70    IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,130)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER URTSRD')
90    FORMAT ('0*** NOTE - BEGIN TO COPY REMAINING  << PREPROCESSOR ',
     *   'COMPONENT TIME SERIES >>.')
100   FORMAT ('0*** ERROR - IN URSTRD - TYPE ',A4,' IN RECORD ',I6,
     *   ' DOES NOT MATCH TYPE IN DATA/TYPE INDEX (',A4,').')
110   FORMAT ('0*** NOTE - ',I4,' TIME SERIES SERIES HAVE BEEN ',
     *   'SUCCESSFULLY COPIED.')
120   FORMAT ('0*** ERROR - IN URTSRD - READING TIME SERIES ',
     *   'AT RECORD ',I6,' OF UNIT ',I2,'.')
130   FORMAT (' *** EXIT URTSRD')
C
      END
