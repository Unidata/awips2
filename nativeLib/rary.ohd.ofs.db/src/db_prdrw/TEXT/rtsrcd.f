C MODULE RTSRCD
C-----------------------------------------------------------------------
C
      SUBROUTINE RTSRCD (ISTRT,ITSID,ITYPE,IUNIT,LWKBUF,IWKBUF,ISTAT)
C
C  THIS ROUTINE READS A TIME SERIES RECORD FROM THE PROCESED DATA BASE.
C  THE HEADER IS READ AND EXPANDED AND THEN ANY ADDITIONAL RECORDS
C  ARE READ.
C
C  ARGUMENT LIST:
C
C       NAME    TYPE  I/O   DIM   DESCRIPTION
C       ------  ----  ---   ---   -----------
C       ISTRT    I     I     1    FIRST LOGICAL RECORD NUMBER
C       ITSID    I     I     2    TIME SERIES ID REQUESTED
C       ITYPE    I     I     1    DATA TYPE REQUESTED
C       IUNIT    I     I     1    LOGICAL UNIT TO READ
C       LWKBUF   I     I     1    DIMENSION OF ARRAY IWKBUF
C       IWKBUF   I    I/O  LWKBUF WORK ARRAY
C       ISTAT    I     O     1    STATUS CODE:
C                                   0=NORMAL RETURN
C                                   1=ARRAY TOO SMALL
C                                   2=TIME SERIES REQUESTED IS
C                                     NOT AT LOCATION ISTRT
C                                   5=FILE READ ERROR
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdatas'
C
      DIMENSION IWKBUF(LWKBUF)
      DIMENSION ITBUF(32),ITSID(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_prdrw/RCS/rtsrcd.f,v $
     . $',                                                             '
     .$Id: rtsrcd.f,v 1.2 1999/07/06 12:59:29 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPRTR.GT.0) WRITE (IOGDB,10) ITSID,ITYPE,ISTRT,IUNIT,LWKBUF
C
      ISTAT=0
C
C  READ TIME SERIES HEADER
      CALL RVLRCD (IUNIT,ISTRT,2,ITBUF,LRECLT,ISTAT)
      IF (ISTAT.NE.0) GO TO 70
C
C  CHECK SIZE OF WORK ARRAY
      IF (LWKBUF.LT.LENHED) THEN
         WRITE (LP,50) LWKBUF,ISTRT,IUNIT,LENHED
         ISTAT=1
         GO TO 70
         ENDIF
C
C  EXPAND HEADER
      CALL PEXPBT (ITBUF,IWKBUF)
C
C  CHECK IF TIME SERIES IS ONE WANTED
C  TIME SERIES ID IS OK IF MATCHES ID IN IWKBUF OR IS BLANK
      IF (ITYPE.EQ.IBLNK) GO TO 30
      IF (ITYPE.NE.IWKBUF(10)) GO TO 20
      CALL UNAMCP (ITSID,IWKBUF(8),IMATCH)
      IF (IMATCH.EQ.0) GO TO 30
      CALL UNAMCP (ITSID,8H        ,IMATCH)
      IF (IMATCH.EQ.0) GO TO 30
C
C  TIME SERIES IS NOT THE ONE WANTED
20    ISTAT=2
      GO TO 70
C
C  CHECK SIZE OF WORK ARRAY
30    NWDS=((IWKBUF(1)+IWKBUF(4)+(LRECLT-1))/LRECLT)*LRECLT
      IF (LWKBUF.LT.NWDS) THEN
         WRITE (LP,50) LWKBUF,ISTRT,IUNIT,NWDS
         ISTAT=1
         GO TO 70
         ENDIF
C
C  MOVE REST OF HEADER INTO WORK ARRAY
      IPOS=LENHED+1
      NUM=LRECLT*2-LENHDC
      CALL UMEMOV (ITBUF(LENHDC+1),IWKBUF(IPOS),NUM)
      IPOS=IPOS+NUM
C
C  GET REST OF TIME SERIES RECORDS
      IXSIZ=IWKBUF(1)-LENHED
      NREC=(IXSIZ+IWKBUF(4)-NUM+LRECLT-1)/LRECLT
      IF (NREC.LE.0) GO TO 70
      IREC=ISTRT+2
      CALL RVLRCD (IUNIT,IREC,NREC,IWKBUF(IPOS),LRECLT,ISTAT)
C      
      IF (ISTAT.NE.0) GO TO 120
C
70    IF (IPRDB.GT.0) THEN
C     PRINT TIME SERIES HEADER
         WRITE (IOGDB,80) (IWKBUF(I),I=1,LENHED)
       
C     PRINT EXTRA BUFFER
         IF (IXSIZ.GT.0) THEN
            IPOS=LENHED+1
            IEND=IPOS+IXSIZ-1
            WRITE (IOGDB,90) (IWKBUF(I),I=IPOS,IEND)
            ENDIF
C     PRINT DATA
         IPOS=IWKBUF(6)
         NUM=IPOS+IWKBUF(3)*IWKBUF(5)-1
         WRITE (IOGDB,110) (IWKBUF(I),I=IPOS,NUM)
         ENDIF
C
120   IF (IPRTR.GT.0) WRITE (IOGDB,130) ISTAT
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' ENTER RTSRCD - ITSID=',2A4,3X,'ITYPE=',A4,3X,
     *   'ISTRT=',I6,3X,'IUNIT=',I3,3X,'LWKBUF=',I6)
50    FORMAT ('0*** ERROR - IN RTSRCD - SIZE OF WORK ARRAY (',I5,
     *   ') TOO SMALL FOR RECORD ',I6,' OF UNIT ',I2,'. ',
     *   I5,' WORDS NEEDED.')
c
c     DR17865
cfan
c     Integer arrays can't be printed out by F format in fortran 90
cfan
cf 80 FORMAT (' HEADER=',7I8,1X,4A4,2F7.2,4I8 / 1X,5A4)
80    FORMAT (' HEADER=',7I8,1X,4A4,x,2I8,x,4I8 / 1X,5A4)    !cfan10/2006
90    FORMAT (' XBUF=',(1X,20A4))
cf110 FORMAT (' DATA=',(1X,12F9.3))
110   FORMAT (' DATA=',(1X,12I9))                            !cfan10/2006
130   FORMAT (' EXIT RTSRCD - ISTAT=',I2)
C
      RETURN
C
      END
