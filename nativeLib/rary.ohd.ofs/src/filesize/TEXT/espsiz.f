C MEMBER ESPSIZ
C  (from old member UZESPSIZ)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/05/95.14:40:34 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE ESPSIZ (USER,DCBDDN,DCBMBR,DSKUNT,MDSTRK,NDSTRK,
     *   XCMD,LPUNCH,ISTAT)
C
C  ROUTINE TO COMPUTE THE NUMBER OF TRACKS FOR EACH OF THE NWSRFS
C  FORECAST SYSTEM EXTENDED PREDICTION SYSTEM DATA BASE DATASETS 
C
      CHARACTER*(*) XCMD,USER,DCBDDN,DCBMBR
      CHARACTER*8 XWORD,XFILE
C
C jgg following added by jto to fix bug r25-12 12/05
      CHARACTER*8 CHAR
      CHARACTER*80 LINE
      CHARACTER DLIM
      DATA DLIM/' '/
      INTEGER CLEN
C end of additions
C
      DIMENSION NDSTRK(MDSTRK),NPTRKS(5)
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'common/eunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/espsiz.f,v $
     . $',                                                             '
     .$Id: espsiz.f,v 1.2 2006/05/09 13:13:07 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
      IPTRKS=0
      CALL UMEMST (0,NPTRKS,5)
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
C  GET DCB
      IF (XCMD.EQ.'ESPP') XFILE='ESPPARM'
      IF (XCMD.EQ.'ESPT') XFILE='ESPTS'
      CALL UFLDCB (DCBDDN,DCBMBR,XFILE,LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 30
C
C  GET NUMBER OF BLOCKS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRBLK=LBLOCK/LRECL
      NPRTRK=NPRBLK*NBLKS
C
C  READ TRACKS CARD
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) GO TO 30
      CALL UFREE (1,72)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
      NFLD=1
      NUM=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NUM)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
      IF (XWORD.EQ.'END') GO TO 30
C
      MAXFLD=0
      IF (XCMD.EQ.'ESPP') MAXFLD=2
      IF (XCMD.EQ.'ESPT') MAXFLD=6
      IF (MAXFLD.EQ.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,40) XCMD
         ISTAT=1
         GO TO 30
         ENDIF
      IF (NFIELD.GT.MAXFLD) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,50)
         ISTAT=1
         GO TO 30
         ENDIF
C
      NUMTSF=NFIELD-1
C
      DO 20 I=1,NUMTSF
         NFLD=I+1
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,60) NFLD
            GO TO 20
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NTRKS)
         IF (NTRKS.GT.0) THEN
            IUNIT=0
            IF (XCMD.EQ.'ESPP') IUNIT=KEPARM
            IF (XCMD.EQ.'ESPT') IUNIT=KEPERM(I)
            NDSTRK(IUNIT)=NTRKS
            IPTRKS=IPTRKS+1
            NPTRKS(IPTRKS)=NTRKS
            IF (XCMD.EQ.'ESPP') THEN
               CALL ULINE (LP,2)
               WRITE (LP,70) XFILE,IUNIT,NTRKS
               ENDIF
            IF (XCMD.EQ.'ESPT') THEN
               CALL ULENTH (XFILE,LEN(XFILE),LENGTH)
               CALL ULINE (LP,2)
               WRITE (LP,80) XFILE(1:LENGTH),I,IUNIT,NTRKS
               ENDIF
            ENDIF
20       CONTINUE
C
      GO TO 10
C
30    IF (ISTAT.EQ.0) THEN
         CALL ULINE (LPUNCH,3)
C jgg following line replaced by jto with below for bug r25-12
C           WRITE (LPUNCH,90) XCMD,'TRACKS',(NPTRKS(I),I=1,IPTRKS)

         WRITE (LPUNCH,100) ' '
         WRITE (LPUNCH,100) XCMD

         LINE=' TRACKS'
         DO 95 I=1,IPTRKS
           CALL KKI2AP(NPTRKS(I),CHAR,CLEN)
           CALL KKCONC(LINE,CHAR,DLIM)
   95      CONTINUE
         WRITE (LPUNCH,100) LINE(1:LENSTR(LINE))
C end of change

         CALL ULINE (LPUNCH,1)
         WRITE (LPUNCH,100) 'END'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('+*** ERROR - COMMAND ',A,' NOT RECOGNIZED.')
50    FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON TRACKS CARD.')
60    FORMAT ('+*** ERROR - INTEGER VALUE EXPECTED IN FIELD ',I2,'.')

C jgg following lines replaced by jto with below for bug r25-12
C 70    FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I3)
C 80    FORMAT ('0',15X,'TRACKS FOR FILE ',A,I1,2X,' UNIT ',I2.2,' = ',I3)
C 90    FORMAT (/ A / A,1X,5(I3,1X))
70    FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I5)
80    FORMAT ('0',15X,'TRACKS FOR FILE ',A,I1,2X,' UNIT ',I2.2,' = ',I5)
C end of changes

100   FORMAT (A)
C
      END
