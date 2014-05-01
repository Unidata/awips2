C MODULE WDGD
C-----------------------------------------------------------------------
C
      SUBROUTINE WDGD (USERID,GOESID,IDTYPE,PPDBID,ISTAT)
C
C
      CHARACTER*8 USERID,GOESID,PPDBID
      CHARACTER*8 USERIDX,GOESIDX,PPDBIDX
      CHARACTER*20 PPDBDS
      CHARACTER*128 FILENAME
C
      DIMENSION DLYTYP(50),RRSTYP(50),ITIME(50)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'dgcommon/dgunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_goes/RCS/wdgd.f,v $
     . $',                                                             '
     .$Id: wdgd.f,v 1.2 1998/04/07 14:30:46 page Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C      
      IF (IDETR.GT.0 ) WRITE (IOGDB,10)
10    FORMAT (' *** ENTER WDGD')
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDGRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,20) FILENAME(1:LENSTR(FILENAME))
20    FORMAT ('0*** ERROR - IN WDGD - CANNOT OPEN FILE ',A,'.')
         ISTAT=1
         GO TO 180
         ENDIF
C
C  GET NUMBER OF ENTRIES
      READ (KDGRCF,REC=1,ERR=60) NHEAD
      IF (NHEAD.EQ.0) THEN
         WRITE (LP,50) FILENAME(1:LENSTR(FILENAME))
50    FORMAT ('0*** NOTE - IN WDGD - NO STATIONS EXIST IN FILE ',A,'.')
         ISTAT=2
         GO TO 170
         ENDIF
      GO TO 80
60    WRITE (LP,70) FILENAME(1:LENSTR(FILENAME))
70    FORMAT ('0*** ERROR - IN WDGD - BAD READ IN FILE ',A,'.')
      ISTAT=1
      GO TO 170
C
C  SEARCH FILE FOR STATION
80    NHEAD=NHEAD+1
      DO 90 NHD=2,NHEAD
         READ (KDGRCF,REC=NHD,ERR=110) USERIDX,
     *      GOESIDX,IDTYPEX,PPDBIDX,PPDBDS,
     *      NDLYTP,(DLYTYP(I),I=1,NDLYTP),
     *      NRRSTP,(RRSTYP(I),I=1,NRRSTP),
     *      NITIME,(ITIME(I),I=1,NITIME)
         IF (USERIDX.EQ.USERID.AND.
     *       GOESIDX.EQ.GOESID.AND.
     *       IDTYPEX.EQ.IDTYPE.AND.
     *       PPDBIDX.EQ.PPDBID) GO TO 130
90       CONTINUE
       IF (IDEDB.GT.0) WRITE (IOGDB,100) USERID,GOESID,
     *     FILENAME(1:LENSTR(FILENAME))
100    FORMAT (' USERID ',A,
     *   ' GOESID ',A,
     *   ' IDTYPE ',I2,
     *   ' PPDBID ',A,
     *   ' NOT FOUND IN FILE ',A)
       ISTAT=3
       GO TO 170
110    WRITE (LP,120) FILENAME(1:LENSTR(FILENAME))
120    FORMAT ('0*** ERROR - IN WDGD - BAD READ OR EOF IN FILE ',A,'.')
       ISTAT=1
       GO TO 170
C
C  STATION FOUND
130   GOESIDX='*DELETED'
      WRITE (KDGRCF,REC=NHD,ERR=150) USERIDX,
     *   GOESIDX,IDTYPEX,PPDBIDX,PPDBDS,
     *   NDLYTP,(DLYTYP(I),I=1,NDLYTP),
     *   NRRSTP,(RRSTYP(I),I=1,NRRSTP),
     *   NITIME,(ITIME(I),I=1,NITIME)
      IF (IDEDB.GT.0) WRITE (IOGDB,140) GOESID,NHD,
     *   FILENAME(1:LENSTR(FILENAME))
140   FORMAT (' STATION ',A,' DELETED AT RECORD ',I6,' IN FILE ',A)
      GO TO 170
150   WRITE (LP,160) FILENAME(1:LENSTR(FILENAME))
160   FORMAT ('0*** ERROR - IN WDGD - BAD WRITE TO FILE ',A,'.')
      ISTAT=1
C
C  CLOSE FILE
170   CALL UPCLOS (KDGRCF,' ',IERR)
C
180   IF (IDETR.GT.0) WRITE (IOGDB,190)
190   FORMAT ('0*** EXIT WDGD')
C
      RETURN
C
      END
