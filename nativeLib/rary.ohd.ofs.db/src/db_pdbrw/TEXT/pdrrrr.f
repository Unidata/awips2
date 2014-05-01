C MODULE PDRRRR
C-----------------------------------------------------------------------
C
      SUBROUTINE PDRRRR (IREC,LRECL,LRRBUF,IRRBUF,ISTAT)
C
C  THIS ROUTINE READS AN RRS RECORD.
C
C  ARGUMENT LIST:
C
C       NAME     TYPE   I/O   DIM   DESCRIPTION
C       ------   ----   ---   ---   -----------
C       IREC       I     I     1    RECORD NUMBER
C       LRECL      I     I     1    RECORD LENGTH
C       LRRBUF     I     I     1    SIZE OF ARRAY IRRBUF
C       IRRBUF     I     O   LRRBUF RRS DATA RECORD ARRAY
C       ISTAT      I     O     1    STATUS INDICATOR:
C                                     0=OK
C                                     1=READ ERROR
C                                     2=ARRAY TOO SMALL
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdi2max'

C
      INTEGER IRRBUF(LRRBUF)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/pdrrrr.f,v $
     . $',                                                             '
     .$Id: pdrrrr.f,v 1.4 2002/05/15 13:44:55 hank Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.1) WRITE (IOGDB,*) 'ENTER PDRRRR'
C
      IF (IPDDB.GT.1) WRITE (IOGDB,*)
     *   ' IREC=',IREC,
     *   ' LRECL=',LRECL,
     *   ' LRRBUF=',LRRBUF
C
      ISTAT=0
C
CMGM 4/2002 THE RRS RECORD NUMBER (IREC) MAY BE STORED IN THE ISIBUF
C    ARRAY AS A NEGATIVE NUMBER. THIS ALLOWS TWICE AS MANY RECORD 
C    NUMBERS IN THE I2 ARRAY. CONVERT IREC TO A POSITIVE RECORD NUMBER
C    IREC2 TO READ/WRITE (UREADT/UWRITT) THE KPDRRS FILE.
CMGM  
	IF(IREC.LT.0)IREC=IREC+2*I2MAX
      IF (LRECL.GT.LRRBUF) THEN
         WRITE (LP,10) IREC,LRECL,LRRBUF
10    FORMAT (' **ERROR** IN PDRRRR - ARRAY TO HOLD RRS RECORD ',I6,
     *   ' TOO SMALL. LRECL=',I6,' LRRBUF=',I6)
         ISTAT=2
         GO TO 30
         ENDIF
C
C  READ FIRST RECORD
      CALL UREADT (KPDRRS,IREC,IRRBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 30
C
      IF (IPDDB.GT.1) WRITE (IOGDB,*) ' IRRBUF(1)=',IRRBUF(1)
C
C  CHECK ARRAY SIZE
      IF (IRRBUF(1).GT.LRRBUF) THEN
         WRITE (LP,20) IREC,IRRBUF(1),LRRBUF
20    FORMAT (' **ERROR** IN PDRRRR - ARRAY TO HOLD RRS RECORD ',I6,
     *   ' TOO SMALL. IRRBUF(1)=',I6,' LRRBUF=',I6)
         CALL UEROR (LP,0,-1)
         ISTAT=2
         GO TO 30
         ENDIF
      NREC=(IRRBUF(1)+LRECL-1)/LRECL-1
      IF (NREC.EQ.0) GO TO 30
      IPOS=LRECL+1
C
C  READ REST OF RECORDS
      CALL RVLRCD (KPDRRS,IREC+1,NREC,IRRBUF(IPOS),LRECL,ISTAT)
C
30    IF (IPDTR.GT.1) WRITE (IOGDB,*) 'EXIT PDRRRR'
C
      RETURN
C
      END
