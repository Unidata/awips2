C MODULE URRDTS
C-----------------------------------------------------------------------
C
      SUBROUTINE URRDTS (ITSID,ITYPE,LTSREC,ITSREC,LFTBUF,IFTBUF,
     *   LAPARM,IAPARM,NUMTS,LTSHDR,ISTAT)
C
C  THIS ROUTINE WILL READ A TIMES SERIES RECORD FROM THE OLD
C  PROCESSED DATA BASE AND WRITES IT TO THE NEW PROCESSED DATA BASE.
C
C  ARGUMENT LIST:
C       NAME      TYPE  I/O   DIM    DESCRIPTION
C       --------  ----  ---  ------  -----------
C       ITSID      A8    I     1     TIME SERIES IDENTIFIER
C       ITYPE      A     I     1     TIME SERIES DATA TYPE
C       LTSREC     I     I     1     LENGTH OF ITSREC
C       ITSREC     I    I/O  LTSREC  TIMES SERIES ARRAY
C       LFTBUF     I     I     1     LENGTH OF IFTBUF
C       IFTBUF     I    I/O  LFTBUF  FUTURE TIME SERIES ARRAY
C       LAPARM     I     I     1     LENGTH OF ARRAY IAPARM
C       IAPARM     I    I/O  LAPARM  PARAMETER RECORD ARRAY
C       LTSHDR     I     O     1     RECORD LOCATION OF TIME SERIES
C       NUMTS      I     O     1     NUMBER OF TIME SERIES PROCESSED
C       ISTAT      I     O     1     STATUS CODE:
C                                      0=NORMAL RETURN
C
      CHARACTER*4 ITYPE,PARMTP
      CHARACTER*8 ITSID
C      
      DIMENSION ITSREC(LTSREC),IAPARM(LAPARM),IFTBUF(LFTBUF)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'urcommon/urcdta'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urrdts.f,v $
     . $',                                                             '
     .$Id: urrdts.f,v 1.5 2003/03/14 18:58:15 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,30)
         ENDIF
C
      ISTAT=0
C
      IFUTHR=0
C      
      IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,40) ITSID,ITYPE
         ENDIF
      IF (ITSID.EQ.'?') THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,40) ITSID,ITYPE
         ENDIF
C
C  FIND DATA TYPE IN DIRECTORY
      IAMORD=0
      CALL PFDTYP (ITYPE,INDX)
      IF (INDX.EQ.0) THEN
         WRITE (LP,60) ITYPE
         CALL SUERRS (LP,2,-1)
         IWURFL=1
         GO TO 20
         ENDIF
C
C  CHECK IF TIME SERIES CREATED BY PREPROCESSOR COMPONENT AND 
C  TYPE IS NOT FMAP WHICH HAS NO PARAMETER RECORD
      IF (DATFIL(11,INDX).EQ.0.AND.ITYPE.NE.'FMAP') THEN         
C     CHECK IF PARAMETER RECORD FOUND
         IAMORD=0
         IPTR=0
         PARMTP=ITYPE
         CALL RPPREC (ITSID,PARMTP,IPTR,LTSREC,ITSREC,NUMFIL,IPTRNX,
     *      IERR)
         IF (IERR.GT.0) THEN
            PARMTP='RRS'
            CALL RPPREC (ITSID,PARMTP,IPTR,LTSREC,ITSREC,NUMFIL,IPTRNX,
     *         IERR)
            IF (IERR.GT.0) THEN
               IF (IERR.EQ.2) THEN
                  WRITE (LP,65) PARMTP,ITYPE,ITSID
                  CALL SUWRNS (LP,2,-1)
                  GO TO 20
                  ENDIF
               CALL SRPPST (ITSID,PARMTP,IPTR,LTSREC,NUMFIL,IPTRNX,
     *            IERR)
               GO TO 20
               ENDIF
            ENDIF
         ENDIF
C
C  READ THE TIME SERIES FROM OLD FILE
      IAMORD=0
      CALL PGETTS (ITSID,ITYPE,LTSREC,ITSREC,IREC,ISTAT)
      IF (ISTAT.GT.0) THEN
         WRITE (LP,50) ITYPE,ITSID
         CALL SUERRS (LP,2,-1)
         IWURFL=1
         GO TO 20
         ENDIF
C         
      IF (IPRDB.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,70) (ITSREC(I),I=1,22)
         ENDIF
C
C  CHECK IF DATA TYPE HAS FUTURE DATA
      IF (DATFIL(7,INDX).LE.0) GO TO 10
C
C  COMPUTE HOUR FUTURE DATA BEGINS
      IFUTHR=ITSREC(14)+ITSREC(5)*ITSREC(2)
C
C  READ FUTURE TIME SERIES
      INDXF=DATFIL(7,INDX)
      IUNIT=DATFIL(2,INDXF)
      IREC=ITSREC(15)
      CALL URDFUT (IREC,IUNIT,IFREC,LFTBUF,IFTBUF,LAPARM,IAPARM,
     *   DATFIL(1,INDXF),NUMTS,IFUTHR,ISTAT)
      IF (ISTAT.NE.0) GO TO 20
C
C  WRITE TIME SERIES TO NEW FILES
10    CALL URWRTS (ITSID,ITYPE,LTSREC,ITSREC,LTSHDR,LAPARM,IAPARM,
     *   NUMTS,IFUTHR,ISTAT)
      IF (ISTAT.EQ.2) THEN
         WRITE (LP,75) ISTAT
         ENDIF
      IF (ISTAT.NE.0 .AND. ISTAT.NE.2) THEN
         IF (IFUTHR.GT.0) THEN
            WRITE (LP,80) ' FUTURE ',ITYPE,ITSID
            CALL SUERRS (LP,2,-1)
            IWURFL=1
            GO TO 20
            ENDIF
         WRITE (LP,80) ' ',ITYPE,ITSID
         CALL SUERRS (LP,2,-1)
         IWURFL=1
         ENDIF
C
20    IF (IPRTR.GT.0) THEN
         CALL SULINE (IOGDB,1)
         WRITE (IOGDB,90)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER URRDTS')
40    FORMAT (' ITSID=',A,3X,'ITYPE=',A)
50    FORMAT ('0*** ERROR - IN URRDTS - ',A,' TIME SERIES FOR ',
     *   'IDENTIFIER ',A,' NOT FOUND.')
60    FORMAT ('0*** ERROR - IN URRDTS - DATA TYPE ',A,' NOT FOUND IN ',
     *   'DATA TYPE DIRECTORY.')
65    FORMAT ('0*** WARNING - IN URRDTS - ',
     *   A,' PARAMETERS FOR DATA TYPE ',A,' ',
     *   'NOT FOUND FOR IDENTIFIER ',A,'. ',
     *   'TIME SERIES WILL NOT BE COPIED.')
70    FORMAT (' FOUND :',7I3,4A4,1X,2F8.2,4I6,5A4)
75    FORMAT ('0**WARNING** IN URRDTS - URWRTS STATUS CODE',I2
     *   ' ENCOUNTERED.')
80    FORMAT ('0*** ERROR - IN URRDTS - WRITING',A,A,
     *   ' TIME SERIES FOR IDENTIFIER ',A,'.')
90    FORMAT (' *** EXIT URRDTS')
C
      END
