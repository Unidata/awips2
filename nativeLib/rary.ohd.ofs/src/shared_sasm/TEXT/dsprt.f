C MODULE DSPRT
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT SASM CONTROL FILE.
C
      SUBROUTINE DSPRT (USERID,PPDBID,ISTAT)
C
      CHARACTER*8 USERID,PPDBID,SAID,SMID
      CHARACTER*8 USERIDX,PPDBIDX
      CHARACTER*20 DESCRP
      CHARACTER*128 FILENAME
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'dscommon/dsunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_sasm/RCS/dsprt.f,v $
     . $',                                                             '
     .$Id: dsprt.f,v 1.7 2002/02/11 20:56:34 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDSRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,10) FILENAME(1:LENSTR(FILENAME))
10    FORMAT ('0*** ERROR - IN DSPRT - CANNOT OPEN FILE ',A,'.')
         ISTAT=1
         GO TO 110
         ENDIF
C
C  GET NUMBER OF STATIONS DEFINED
      READ (KDSRCF,REC=1,IOSTAT=IERR) NHEAD
      IF (IERR.GT.0) THEN
         WRITE (LP,20) IERR,FILENAME(1:LENSTR(FILENAME))
20    FORMAT ('0*** ERROR - IN DSPRT - IOSTAT=',I5,' READING ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 100
         ENDIF
      IF (IERR.LT.0) THEN
         WRITE (LP,30) FILENAME(1:LENSTR(FILENAME))
30    FORMAT ('0*** ERROR - IN DSPRT - EOF WHILE READING RECORD 1 OF ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 100
         ENDIF
      IF (NHEAD.LE.0) THEN
         WRITE (LP,40)
40    FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
         ISTAT=2
         GO TO 100
         ENDIF
C
C  PROCESS EACH STATION
      NUM=0
      DO 80 NHD=1,NHEAD
         LHEAD=NHD+1
         READ (KDSRCF,REC=LHEAD,IOSTAT=IERR) USERIDX,
     *      SAID,SMID,PPDBIDX,
     *      DESCRP,STALAT,IDTPP,IDTTA,IPE,ISD,ISW
         IF (IERR.NE.0) THEN
            WRITE (LP,50) LHEAD,FILENAME(1:LENSTR(FILENAME))
50    FORMAT ('0*** ERROR - IN DSPRT - BAD READING RECORD ',I7,' IN ',
     *   'FILE ',A,'.')
            ISTAT=1
            GO TO 100
            ENDIF
         IF (PPDBID.NE.' '.AND.PPDBID.NE.PPDBIDX) GO TO 80
         IF (NUM.EQ.0) THEN
            WRITE (LP,60)
60    FORMAT ('0',4X,3X,
     *   'USERID  ',3X,
     *   'SAID    ',3X,
     *   'SMID    ',3X,
     *   'PPDBID  ',3X,
     *   'DESCRP              ',3X,
     *   'STALAT',3X,
     *   'IDTPP',3X,
     *   'IDTTA',3X,
     *   'IPE  ',3X,
     *   'ISD  ',3X,
     *   'ISW')
            CALL SULINE (LP,2)
            WRITE (LP,65)
65    FORMAT (' ',4X,3X,
     *   '--------',3X,
     *   '--------',3X,
     *   '--------',3X,
     *   '--------',3X,
     *   '--------------------',3X,
     *   '------',3X,
     *   '-----',3X,
     *   '-----',3X,
     *   '-----',3X,
     *   '-----',3X,
     *   '-----')
            CALL SULINE (LP,1)
            ENDIF
         WRITE (LP,70) NHD,USERIDX,
     *      SAID,SMID,PPDBIDX,DESCRP,STALAT,
     *      IDTPP,IDTTA,IPE,ISD,ISW
70       FORMAT (' ',I4,3X,4(A,3X),A,3X,F6.1,3X,5(I5,3X))
         CALL SULINE (LP,1)
         NUM=NUM+1
80       CONTINUE
C
      IF (NUM.EQ.0) THEN
         IF (PPDBID.EQ.' ') THEN
            ISTAT=3
            ELSE
               WRITE (LP,90) PPDBID
90    FORMAT ('0*** NOTE - STATION ',A,' NOT FOUND IN SASM CONTROL ',
     *   'FILE.')
               CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
100   CALL UPCLOS (KDSRCF,' ',IERR)
C
110   RETURN
C
      END
