C MODULE WSDCH
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHANGE A STATION IN THE SASM CONTROL FILE.

      SUBROUTINE WDSCH (USERID,SAID,SMID,PPDBID,DESCRP,STALAT,
     *   IDTPP,IDTTA,IPE,ISD,ISW,ISTAT)
C
      CHARACTER*8 USERID,SAID,SMID,PPDBID
      CHARACTER*8 USERIDX,SAIDX,SMIDX,PPDBIDX
      CHARACTER*20 DESCRP
      CHARACTER*128 FILENAME
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'dscommon/dsunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_sasm/RCS/wdsch.f,v $
     . $',                                                             '
     .$Id: wdsch.f,v 1.3 2000/12/18 22:52:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
      IF (IDETR.GT.0) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER WSDCH')
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDSRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,20) FILENAME(1:LENSTR(FILENAME))
20    FORMAT ('0*** ERROR - IN WSDCH - CANNOT OPEN FILE ',A,'.')
         ISTAT=1
         GO TO 150
         ENDIF
C
C  GET NUMBER OF STATION DEFINED
      READ (KDSRCF,REC=1,IOSTAT=IERR) NHEAD
      IF (IERR.NE.0) THEN
         WRITE (LP,30) IERR,FILENAME(1:LENSTR(FILENAME))
30    FORMAT ('0*** ERROR - IN WDSCH - IOSTAT=',I5,' READING ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 140
         ENDIF
      IF (IERR.LT.0) THEN
         WRITE (LP,40) FILENAME(1:LENSTR(FILENAME))
40    FORMAT ('0*** ERROR - IN WDSCH - EOF WHILE READING RECORD 1 OF ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 140
         ENDIF
      IF (NHEAD.LE.0) THEN
         IF (IDEDB.GT.0) WRITE (IOGDB,50) FILENAME(1:LENSTR(FILENAME))
50    FORMAT (' NO STATIONS ARE DEFINED IN FILE ',A)
         ISTAT=2
         GO TO 140
         ENDIF
C
C  SEARCH FILE FOR STATION
      NHEAD=NHEAD+1
      DO 60 NHD=2,NHEAD
         READ (KDSRCF,REC=NHD,ERR=80) USERIDX,SAIDX,SMIDX,PPDBIDX
         IF (USERIDX.EQ.USERID.AND.
     *       PPDBIDX.EQ.PPDBID) GO TO 100
60       CONTINUE
      IF (IDEDB.GT.0) WRITE (IOGDB,70) USERID,PPDBID,
     *   FILENAME(1:LENSTR(FILENAME))
70    FORMAT (' USERID ',A,
     *   ' AND PPDBID ',A,
     *   ' NOT FOUND IN FILE ',A)
      ISTAT=3
      GO TO 140
C
80    WRITE (LP,90) FILENAME(1:LENSTR(FILENAME))
90    FORMAT ('0*** ERROR - IN WSDCH - BAD READ OR EOF IN FILE ',A)
      ISTAT=1
      GO TO 140
C
100   WRITE (KDSRCF,REC=NHD,ERR=120) USERID,SAID,SMID,PPDBID,
     *   DESCRP,STALAT,IDTPP,IDTTA,IPE,ISD,ISW
      IF (IDEDB.GT.0) WRITE (IOGDB,110) PPDBID,NHD,
     *   FILENAME(1:LENSTR(FILENAME))
110   FORMAT (' PPDBID ',A,' CHANGED AT RECORD ',I5,' OF FILE ',A)
      GO TO 140
C
120   WRITE (LP,130) NHD,FILENAME(1:LENSTR(FILENAME))
130   FORMAT ('0*** ERROR - IN WDSCH - BAD WRITE AT RECORD ',I5,
     *   ' IN FILE ',A,'.')
      ISTAT=1
C
140   CALL UPCLOS (KDSRCF,' ',IERR)
C
150   IF (IDETR.GT.0) WRITE (IOGDB,160)
160   FORMAT (' *** EXIT WDSCH')
C
      RETURN
C
      END
