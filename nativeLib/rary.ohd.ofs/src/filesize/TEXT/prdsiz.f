C MEMBER UZPRDSIZ
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/02/95.11:28:57 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRDSIZ (USER,XCMD,DCBDDN,DCBMBR,DSKUNT,MDSTRK,NDSTRK,
     *   LDEBUG,LPUNCH,ISTAT)
C
C  ROUTINE TO COMPUTE THE NUMBER OF TRACKS FOR EACH OF THE NWSRFS
C  FORECAST SYSTEM PROCESSED DATA BASE DATASETS.
C
      PARAMETER (MTYPEC=200)
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR,XCMD
      CHARACTER*4 XTYPE,ZTYPE
      CHARACTER*4 DIMN,UNIT,TIME
      CHARACTER*4 TYPEC(MTYPEC)
      CHARACTER*8 XWORD
C
      CHARACTER*8 XFILE
C
C jgg following added by jto to fix bug r25-12 12/05
      CHARACTER*8 CHAR
      CHARACTER*80 LINE
      CHARACTER DLIM
      DATA DLIM/' '/
      INTEGER CLEN
C end of additions

      DIMENSION NDSTRK(MDSTRK)
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/prdsiz.f,v $
     . $',                                                             '
     .$Id: prdsiz.f,v 1.4 2006/05/09 13:13:48 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C      
      ICDBEG=1
      ICDEND=72
C
      NTYPEC=0
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
      NTTRKS=0
      MAXTMS=0
      MINDAY=0
      NINDEX=0
C
C  FILE DATA TYPE COMMON BLOCK
      DIMN='NONE'
      UNIT='NONE'
      MISS=-99
      NVAL=-99
      TIME='NONE'
      NADD=-99
      IWRTF=-99
      CALL UDTYPE ('FCST',MTYPEC,NTYPEC,TYPEC,DIMN,UNIT,MISS,NVAL,
     *   TIME,NADD,IWRTF,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  GET ATTRIBUTES FOR PARAMETER FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PRDPARM',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 80
C      
C  CALCULATE NUMBER OF RECORDS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 80      
      NPRBLK=LBLOCK/LRECL
      RECTRK=NPRBLK*NBLKS
C
C  COMPUTE TRACK SIZE FOR PARAMETER FILE
      IUNIT=KRFCPR
      NWORDS=LRECL/4
      NREC=(MAXDTP*NWDDTP)/NWORDS
      IF (NREC*NWORDS.LT.MAXDTP*NWDDTP) NREC=NREC+1
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*)
     *      ' LRECL=',LRECL,
     *      ' NWORDS=',NWORDS,
     *      ' MAXDTP=',MAXDTP,
     *      ' NWDDTP=',NWDDTP,
     *      ' NREC=',NREC,
     *      ' RECTRK=',RECTRK,
     *      ' '
         ENDIF
      XTRKS=NREC/RECTRK
      NXTRKS=XTRKS+.99
      NDSTRK(IUNIT)=NXTRKS
      CALL ULINE (LP,2)
      WRITE (LP,130) 'PRDPARM ',IUNIT,NXTRKS      
C
C  GET ATTRIBUTES FOR TIME SERIES FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PRDTS',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  CALCULATE NUMBER OF RECORDS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 80
      NPRBLK=LBLOCK/LRECL
      RECTRK=NPRBLK*NBLKS
C
C  READ FILE NAME
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) GO TO 80
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD=',XWORD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
C
C  CHECK FOR END CARD
      IF (XWORD.EQ.'END') GO TO 60
C
C  CHECK FOR 'MINDAY' CARD (OPTIONAL)
      IF (XWORD.EQ.'MINDAY'.AND.NCHAR.EQ.6) THEN
         IF (NFIELD.NE.2) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,220) 'MINDAY'
            GO TO 80
            ENDIF
         NFLD=2
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,230) NFLD
            GO TO 80
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),MINDAY)
         GO TO 10
         ENDIF
C
C  CHECK FOR VALID FILE NAME
      XFILE=XWORD
      IF (XFILE.EQ.'PRDTS1') GO TO 20
      IF (XFILE.EQ.'PRDTS2') GO TO 20
      IF (XFILE.EQ.'PRDTS3') GO TO 20
      IF (XFILE.EQ.'PRDTS4') GO TO 20
      IF (XFILE.EQ.'PRDTS5') GO TO 20
      CALL UEROR (LP,1,-1)
      WRITE (LP,100) 'FILE NAME',XFILE
      GO TO 10
C
C  COMPUTE FILE SIZE
20    FTRKS=0.
C
C  READ CARD
30    CALL RPCARD (IBUF,IERR)
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) GO TO 80
      CALL UFREE (ICDBEG,ICDEND)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 30
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 30
C
C  CHECK FOR END OF TYPES FOR FILE
      IF (XWORD.EQ.'END') THEN
         NTRKS=FTRKS+.99
         IF (NTRKS.EQ.0) NTRKS=1
         IF (XFILE.EQ.'PRDTS1') IUNIT=KMAPTS
         IF (XFILE.EQ.'PRDTS2') IUNIT=KGENTS
         IF (XFILE.EQ.'PRDTS3') IUNIT=KFMPTS
         IF (XFILE.EQ.'PRDTS4') IUNIT=KFUTTS
         IF (XFILE.EQ.'PRDTS5') IUNIT=KFCTTS
         CALL ULINE (LP,2)
         WRITE (LP,130) XFILE,IUNIT,NTRKS
         NDSTRK(IUNIT)=NDSTRK(IUNIT)+NTRKS
         NTTRKS=NTTRKS+NTRKS
         GO TO 10
         ENDIF
C
C  SET TIME SERIES TYPE
      XTYPE=XWORD
C
C  CHECK FOR MISCELLANEOUS SPACE ALLOCATION
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*)
     *      ' XTYPE=',XTYPE,
     *      ' NFIELD=',NFIELD,
     *      ' '
         ENDIF
      IF (XTYPE.EQ.'MISC'.AND.NFIELD.EQ.2) THEN
         NFLD=2
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IPCT)
         TRKS=FTRKS*(IPCT/100.)
         IF (LDEBUG.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,*)
     *         ' IPCT=',IPCT,
     *         ' TRKS=',TRKS,
     *         ' FTRKS=',FTRKS,
     *         ' '
            ENDIF
         FTRKS=FTRKS+TRKS
         CALL ULINE (LP,1)
         WRITE (LP,120) XTYPE,TRKS
         GO TO 30
         ENDIF
C
      IF (XTYPE.EQ.'MISC') GO TO 50
      ZTYPE=XTYPE
      IF (XTYPE.EQ.'FMAP') ZTYPE='MAP'
C
C  CHECK FOR VALID TYPE
      DO 40 I=1,NTYPEC
         IF (ZTYPE.EQ.TYPEC(I)) GO TO 50
40       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,100) 'DATA TYPE',XTYPE
C
C  GET TIME INTERVAL
50    NFLD=2
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),ITIME)
C
C  GET NUMBER OF VALUES PER TIME INTERVAL
      NFLD=NFLD+1
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NVAL)
C
C  GET NUMBER OF DAYS OF DATA
      NFLD=NFLD+1
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NDAYS)
C
C  GET NUMBER OF TIME SERIES
      NFLD=NFLD+1
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NUMTS)
C
      DTRKS=0.
C
      IF (ITIME.EQ.0.OR.NVAL.EQ.0.OR.NDAYS.EQ.0.OR.NUMTS.EQ.0) GO TO 30
C
C  COMPUTE NUMBER OF TRACKS FOR FILE
      LHEAD=18
      NWORDS=LHEAD+24/ITIME*NVAL*NDAYS
      NRECS=(NWORDS-1)/(LRECL/4)+1
      DTRKS=NRECS*NUMTS/RECTRK
      TRACK2=(((24/ITIME*NVAL*NDAYS+18-1)/(LRECL/4)+1)*NUMTS)/RECTRK
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'NWORDS=',NWORDS,
     *      ' NRECS=',NRECS,
     *      ' DTRKS=',DTRKS,
     *      ' TRACK2=',TRACK2
         ENDIF
C
      MAXTMS=MAXTMS+NUMTS
      FTRKS=FTRKS+DTRKS
C
      CALL ULINE (LP,1)
      WRITE (LP,120) XTYPE,DTRKS
      GO TO 30
C
60    CALL ULINE (LP,2)
      WRITE (LP,140) MAXTMS
C
C  GET ATTRIBUTES FOR INDEX FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PRDINDEX',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  CALCULATE NUMBER OF RECORDS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 80
      NPRBLK=LBLOCK/LRECL
      RECTRK=NPRBLK*NBLKS
C
C  CALCULATE NUMBER OF TRACKS FOR INDEX FILE
      FTRKS=(MAXTMS*2/RECTRK)+.99
      NTRKS=FTRKS
      NTTRKS=NTTRKS+NTRKS
      IUNIT=KINDEX
      NDSTRK(IUNIT)=NTRKS
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL ULINE (LP,2)
      WRITE (LP,130) 'PRDINDEX',IUNIT,NTRKS
      NINDEX=NTRKS
C
C  CHECK IF ALL UNITS HAVE AT LEAST 1 TRACK ALLOCATED
      DO 70 I=1,NMPRDF
         IF (NDSTRK(KPRDTU(I)).GT.0) GO TO 70
            NDSTRK(KPRDTU(I))=1
            CALL ULINE (LP,2)
            WRITE (LP,150) KPRDTU(I),NDSTRK(KPRDTU(I))
70      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,160) NTTRKS
      GO TO 90
C
80    ISTAT=1
C
90    IF (ISTAT.EQ.0) THEN
C
         CALL ULINE (LPUNCH,2)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,170) XCMD
         WRITE (LPUNCH,210) ' '
         WRITE (LPUNCH,210) XCMD
C end of changes
	 
         IF (MINDAY.EQ.0) THEN
            CALL UWARN (LP,1,-1)
            WRITE (LP,180)
            CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C             WRITE (LPUNCH,190) 'MINDAY','?'
            WRITE (LPUNCH,210) 'MINDAY ?'
C end of changes

            ELSE
               CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C                WRITE (LPUNCH,200) 'MINDAY',MINDAY
               LINE=' MINDAY'
               CALL KKI2AP(MINDAY,CHAR,CLEN)
               CALL KKCONC(LINE,CHAR,DLIM)
               WRITE (LPUNCH,210) LINE(1:LENSTR(LINE))
C end of changes
            ENDIF
         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,210) 'TRACKS',(NDSTRK(KPRDTU(I)),I=1,NMPRDF)
         LINE=' TRACKS'
         DO 95 I=1,NMPRDF
           CALL KKI2AP(NDSTRK(KPRDTU(I)),CHAR,CLEN)
           CALL KKCONC(LINE,CHAR,DLIM)
   95      CONTINUE
         WRITE (LPUNCH,210) LINE(1:LENSTR(LINE))
C end of changes

         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C           WRITE (LPUNCH,200) 'INDEX',NINDEX
         LINE=' INDEX'
         CALL KKI2AP(NINDEX,CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
         WRITE (LPUNCH,210) LINE(1:LENSTR(LINE))
C end of changes

         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,190) 'END'
         WRITE (LPUNCH,210) ' END'
C end of changes
         ENDIF
C
      RETURN
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('+*** ERROR - INVALID ',A,' : ',A)
120   FORMAT (' ',15X,'TRACKS FOR TYPE ',A,' = ',F7.2)
C jgg following line replaced by jto with below for bug r25-12
C 130   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I3)
130   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I5)

140   FORMAT ('0TOTAL TIME SERIES = ',I5)
C jgg following line replaced by jto with below for bug r25-12
C 150   FORMAT ('0*** NOTE - TRACKS FOR UNIT ',I2.2,' SET TO ',I3,'.')
150   FORMAT ('0*** NOTE - TRACKS FOR UNIT ',I2.2,' SET TO ',I5,'.')

160   FORMAT ('0TOTAL TRACKS = ',I5)
C jgg following line removed by jto with below for bug r25-12
C 170   FORMAT (/ A)

180   FORMAT ('+*** WARNING - MINDAY NOT SPECIFIED AND WILL ',
     *        'BE SET TO ?.')
C jgg following line replaced by jto with below for bug r25-12
C 190   FORMAT (A,1X,A)
C 200   FORMAT (A,1X,I3)
C 210   FORMAT (A,1X,5(I3,1X))
210   FORMAT (A)
C end of changes
220   FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON ',A,' CARD.')
230   FORMAT ('+*** ERROR - FIELD ',I2,' MUST BE AN INTEGER.')
C
      END
