C MODULE SFDECK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK IF STATION IS DEFINED IN DATA ENTRY CONTROL FILES.
C
      SUBROUTINE SFDECK (PPDBIDX,SRCCD,IGOES,ISASM,IFOUND,ISTAT)
C
      CHARACTER*4 SRCCD
      CHARACTER*8 USERID,PPDBID,PPDBIDX,SAID,SMID
      CHARACTER*20 DESCRP
      CHARACTER*4 DLYTYP(25),RRSTYP(25)
      CHARACTER*8 STAID
      CHARACTER*128 FILENAME
      DIMENSION ITIME(50)
C
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'dgcommon/dgunts'
      INCLUDE 'dscommon/dsunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfdeck.f,v $
     . $',                                                             '
     .$Id: sfdeck.f,v 1.1 1998/07/13 20:05:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('STA ')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STA ')
C
      ISTAT=0
C
      IFOUND=0      
C
      IF (IGOES.EQ.1) THEN
C     OPEN FILE
         IOPEN=2
         CALL UDOPEN (KDGRCF,IOPEN,FILENAME,LRECL,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,50) FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
C     GET NUMBER OF STATIONS DEFINED
         READ (KDGRCF,REC=1,IOSTAT=IERR) NHEAD
         IF (IERR.NE.0) THEN
            WRITE (LP,60) IERR,FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
         IF (IERR.LT.0) THEN
            WRITE (LP,70) FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
          IF (NHEAD.LE.0) THEN
             ISTAT=1
             GO TO 30
             ENDIF
C     SET PLATFORM TYPE             
         IDTYPEX=-1
         IF (SRCCD.EQ.'GHB5') IDTYPEX=0
         IF (SRCCD.EQ.'GPLT') IDTYPEX=1
         IF (SRCCD.EQ.'CDAS') IDTYPEX=2
         IF (IDTYPEX.EQ.-1) THEN
            WRITE (LP,75) SRCCD,STAID
            CALL SUERRS (LP,2,NUMERR)
            GO TO 30
            ENDIF
C     PROCESS EACH STATION
         IFOUND=1
         DO 10 NHD=1,NHEAD
            LHEAD=NHD+1
            READ (KDGRCF,REC=LHEAD,IOSTAT=IERR) USERID,
     *         STAID,IDTYPE,PPDBID,DESCRP,
     *         NDLYTP,(DLYTYP(I),I=1,NDLYTP),
     *         NRRSTP,(RRSTYP(I),I=1,NRRSTP),
     *         NITIME,(ITIME(I),I=1,NITIME)
            IF (IERR.NE.0) THEN
               WRITE (LP,80) LHEAD,FILENAME
               CALL SUERRS (LP,2,-1)
               ISTAT=1
               GO TO 30
               ENDIF
C        CHECK IF DELETED
            IF (STAID.EQ.'*DELETED') GO TO 10
C        CHECK IF SPECIFIED IDENTIFIER AND TYPE
            IF (PPDBIDX.EQ.PPDBID.AND.
     *          IDTYPEX.EQ.IDTYPE) GO TO 30
10          CONTINUE
         IFOUND=0
         GO TO 30
         ENDIF
C
      IF (ISASM.EQ.1) THEN
C     OPEN FILE
         IOPEN=2
         CALL UDOPEN (KDSRCF,IOPEN,FILENAME,LRECL,IERR)
         IF (IERR.NE.0) THEN
            WRITE (LP,50) FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
C     GET NUMBER OF STATIONS DEFINED
         READ (KDSRCF,REC=1,IOSTAT=IERR) NHEAD
         IF (IERR.GT.0) THEN
            WRITE (LP,60) IERR,FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
         IF (IERR.LT.0) THEN
            WRITE (LP,70) FILENAME(1:LENSTR(FILENAME))
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 30
            ENDIF
         IF (NHEAD.LE.0) THEN
            ISTAT=1
            GO TO 30
            ENDIF
C     PROCESS EACH STATION
         IFOUND=1
         DO 20 NHD=1,NHEAD
            LHEAD=NHD+1
            READ (KDSRCF,REC=LHEAD,IOSTAT=IERR) USERID,
     *         SAID,SMID,PPDBID,
     *         DESCRP,STALAT,IDTPP,IDTTA,IPE,ISD,ISW
            IF (IERR.NE.0) THEN
               WRITE (LP,80) LHEAD,FILENAME(1:LENSTR(FILENAME))
               CALL SUERRS (LP,2,-1)
               ISTAT=1
               GO TO 30
               ENDIF
            IF (PPDBIDX.EQ.PPDBID) GO TO 30
20          CONTINUE
         IFOUND=0
         GO TO 30
         ENDIF
C
30    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,90) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SFDECK')
50    FORMAT ('0*** WARNING - IN SFDECK - CANNOT OPEN FILE ',A,'.')
60    FORMAT ('0*** ERROR - IN SFDECK - IOSTAT=',I5,' READING ',
     *   'FILE ',A,'.')
70    FORMAT ('0*** ERROR - IN SFDECK - EOF WHILE READING RECORD 1 OF ',
     *   'FILE ',A,'.')
75    FORMAT ('0*** ERROR - IN SFDECK - GOES IDENTIFIER ''',A,''' ',
     *    'IS INVALID FOR STATION ',A,'.')
80    FORMAT (' *** ERROR - IN SFDECK - BAD READING RECORD ',I7,' IN ',
     *   'FILE ',A,'.')
90    FORMAT (' *** EXIT SFDECK : ISTAT=',I2)
C
      END
