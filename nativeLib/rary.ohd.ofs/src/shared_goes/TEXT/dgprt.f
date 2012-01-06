C MODULE DGPRT
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT GOES CONTROL FILE.
C
      SUBROUTINE DGPRT (USERID,PPDBID,ISTAT)
C
      CHARACTER*4  DLYTYP(25),RRSTYP(25)
      CHARACTER*8  USERID,PPDBID,STAID
      CHARACTER*8  USERIDX,PPDBIDX
      CHARACTER*20 DESCRP
      CHARACTER*128 FILENAME
      DIMENSION ITIME(50)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'dgcommon/dgunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_goes/RCS/dgprt.f,v $
     . $',                                                             '
     .$Id: dgprt.f,v 1.5 2002/02/11 20:46:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
C  OPEN FILE
      IOPEN=2
      CALL UDOPEN (KDGRCF,IOPEN,FILENAME,LRECL,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,10) FILENAME(1:LENSTR(FILENAME))
10    FORMAT ('0*** ERROR - IN DGPRT - CANNOT OPEN FILE ',A,'.')
         ISTAT=1
         GO TO 140
         ENDIF
C
C  GET NUMBER OF STATIONS DEFINED
      READ (KDGRCF,REC=1,IOSTAT=IERR) NHEAD
      IF (IERR.NE.0) THEN
         WRITE (LP,20) IERR,FILENAME(1:LENSTR(FILENAME))
20    FORMAT ('0*** ERROR - IN DGPRT - IOSTAT=',I5,' READING ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 130
         ENDIF
      IF (IERR.LT.0) THEN
         WRITE (LP,30) FILENAME(1:LENSTR(FILENAME))
30    FORMAT ('0*** ERROR - IN DGPRT - EOF WHILE READING RECORD 1 OF ',
     *   'FILE ',A,'.')
         ISTAT=1
         GO TO 130
         ENDIF
       IF (NHEAD.EQ.0) THEN
          WRITE (LP,40)
40     FORMAT ('0*** NOTE - NO STATIONS ARE DEFINED.')
          ISTAT=2
          GO TO 130
          ENDIF
C
C  PROCESS EACH STATION
      NUM=0
      DO 110 NHD=1,NHEAD
         LHEAD=NHD+1
         READ (KDGRCF,REC=LHEAD,IOSTAT=IERR) USERIDX,
     *      STAID,IDTYPE,PPDBIDX,DESCRP,
     *      NDLYTP,(DLYTYP(I),I=1,NDLYTP),
     *      NRRSTP,(RRSTYP(I),I=1,NRRSTP),
     *      NITIME,(ITIME(I),I=1,NITIME)
         IF (IERR.NE.0) THEN
            WRITE (LP,50) LHEAD,FILENAME(1:LENSTR(FILENAME))
50    FORMAT ('0*** ERROR - IN DGPRT - BAD READING RECORD ',I7,' IN ',
     *   'FILE ',A,'.')
            ISTAT=1
            GO TO 130
            ENDIF
C     CHECK IF DELETED            
         IF (STAID.EQ.'*DELETED') GO TO 110 
C     CHECK IF SPECIFIED IDENTIFIER      
         IF (PPDBID.NE.' '.AND.PPDBID.NE.PPDBIDX) GO TO 110
         IF (NUM.EQ.0) THEN
            WRITE (LP,60)
60    FORMAT ('0',4X,3X,
     *   'USERID  ',3X,
     *   'STAID   ',3X,
     *   'IDTYPE',3X,
     *   'PPDBID  ',3X,
     *   'DESCRP              ',3X,
     *   'DATA_TYPE-DATA_TIME_INTERVAL')
            CALL SULINE (LP,2)
            WRITE (LP,65)
65    FORMAT (' ',4X,3X,
     *   '--------',3X,
     *   '--------',3X,
     *   '------',3X,
     *   '--------',3X,
     *   '--------------------',3X,
     *   '----------------------------')
            CALL SULINE (LP,1)
            ENDIF
         NTYPE=NDLYTP
         NPER=5
         IF (NTYPE.GT.NPER) NTYPE=NPER
         WRITE (LP,70) NHD,USERIDX,
     *      STAID,IDTYPE,PPDBIDX,DESCRP,
     *      (DLYTYP(I),ITIME(I),I=1,NTYPE)
70       FORMAT (' ',I4,3X,2(A,3X),I6,3X,A,3X,A,3X,
     *      5(A,'-',I4.4,3X))
         CALL SULINE (LP,1)
         IF (NDLYTP.GT.NPER) THEN
            NTIME=(NDLYTP-NPER)/NPER
            IF (MOD(NDLYTP,NPER).NE.0) NTIME=NTIME+1
            NUM1=NPER+1
            NUM2=NPER*2
            DO 80 J=1,NTIME
               IF (NUM2.GT.NDLYTP) NUM2=NDLYTP
               WRITE (LP,90) (DLYTYP(I),ITIME(I),I=NUM1,NUM2)
               CALL SULINE (LP,1)
               NUM1=NUM1+NPER
               NUM2=NUM2+NPER
80          CONTINUE
            ENDIF
         NTYPE=NRRSTP
         NPER=5
         IF (NTYPE.GT.NPER) NTYPE=NPER
         IF (NTYPE.NE.0) THEN
            WRITE (LP,90) (RRSTYP(I),ITIME(NDLYTP+I),I=1,NTYPE)
90          FORMAT (' ',T73,5(A,'-',I4.4,3X))
            CALL SULINE (LP,1)
            IF (NRRSTP.GT.NPER) THEN
               NTIME=(NRRSTP-NPER)/NPER
               IF (MOD(NRRSTP,NPER).NE.0) NTIME=NTIME+1
               NUM1=NPER+1
               NUM2=NPER*2
               DO 100 J=1,NTIME
                  IF (NUM2.GT.NRRSTP) NUM2=NRRSTP
                  WRITE (LP,90) (DLYTYP(I),ITIME(NDLYTP+I),I=NUM1,NUM2)
                  CALL SULINE (LP,1)
                  NUM1=NUM1+NPER
                  NUM2=NUM2+NPER
100            CONTINUE
               ENDIF
            ENDIF
         NUM=NUM+1
110      CONTINUE
C
      IF (NUM.EQ.0) THEN
         IF (PPDBID.EQ.' ') THEN
            ISTAT=3
            ELSE
               WRITE (LP,120) PPDBID
120   FORMAT ('0*** NOTE - STATION ',A,' NOT FOUND IN GOES CONTROL ',
     *   'FILE.')
               CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
130   CALL UPCLOS (KDGRCF,' ',IERR)
C
140   RETURN
C
      END
