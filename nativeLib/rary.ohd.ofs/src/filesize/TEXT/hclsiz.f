C MEMBER HCLSIZ
C  (from old member UZHCLSIZ)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/05/95.14:44:19 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCLSIZ (USER,XCMD,DCBDDN,DCBMBR,DSKUNT,MDSTRK,NDSTRK,
     *   LPUNCH,ISTAT)
C
C  ROUTINE TO COMPUTE THE NUMBER OF TRACKS FOR EACH OF THE NWSRFS
C  FORECAST SYSTEM HYDROLOGIC COMMAND LANGUAGE DATA BASE DATASETS.
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR,XCMD
      CHARACTER*8 XWORD,XTYPE
      CHARACTER*4 PASSWD,OPTN

C jgg following added by jto to fix bug r25-12 12/05
      CHARACTER*8 CHAR
      CHARACTER*80 LINE
      CHARACTER DLIM
      DATA DLIM/' '/
      INTEGER CLEN
C end of additions

      DIMENSION NDSTRK(MDSTRK)
      DIMENSION NPTRKS(3)
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'hclcommon/hunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/hclsiz.f,v $
     . $',                                                             '
     .$Id: hclsiz.f,v 1.2 2006/05/09 13:13:38 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
      CALL UMEMST (0,NPTRKS,3)
C
C  PRINT CARD
      CALL WPCARD (IBUF)
C
      NTTRKS=0
      IPTRKS=0
      IPRINT=2
      OPTN='?'
      PASSWD='?'
C
C  READ FILE TYPE
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) GO TO 30
      CALL UFREE (1,72)
C
C  CHECK IF BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
C
C  CHECK FOR END CARD
      IF (XWORD.EQ.'END') GO TO 50
C  CHECK FOR OPITIONAL PASSWORD CARD
      IF (XWORD.EQ.'PASSWORD'.AND.NCHAR.EQ.8) GO TO 40
C
      XTYPE=XWORD
C
C  CHECK FOR VALID FILE TYPE
      IF (XTYPE.EQ.'GLOBAL'.OR.XTYPE.EQ.'LOCAL') THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,60) XTYPE
            GO TO 10
            ENDIF
C
       IF (NFIELD.EQ.2.AND.XTYPE.EQ.'GLOBAL') THEN
          NFLD=2
          NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
          CALL UPACK1 (IBUF(IFSTRT(NFLD)),OPTN,NCHAR)
          IF (OPTN.EQ.'OLD'.OR.OPTN.EQ.'NEW') GO TO 20
             CALL UEROR (LP,1,-1)
             WRITE (LP,160) OPTN
             GO TO 10
          ENDIF
C  GET DCB FOR INDEX DATASET
20    CALL UFLDCB (DCBDDN,DCBMBR,'HCLINDEX',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRBLK=LBLOCK/LRECL
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRTRK=NPRBLK*NBLKS
      NRECS=4004
      CALL ULINE (LP,2)
      WRITE (LP,70) NRECS,'HCLINDEX'
      TRKS=1.*NRECS/NPRTRK
      NTRKS=TRKS
      IF (TRKS-NTRKS.GT.0) NTRKS=NTRKS+1
      IUNIT=0
      IF (XTYPE.EQ.'GLOBAL') IUNIT=KINDXG
      IF (XTYPE.EQ.'LOCAL') IUNIT=KINDXL
      NDSTRK(IUNIT)=NTRKS
      CALL ULINE (LP,2)
      WRITE (LP,80) 'HCLINDEX',IUNIT,NTRKS
      NTTRKS=NTTRKS+NTRKS
      NPTRKS(IPTRKS+1)=NTRKS
      IPTRKS=IPTRKS+1
C
C  GET DCB FOR DEFINITION DATASET
      CALL UFLDCB (DCBDDN,DCBMBR,'HCLDEFIN',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRBLK=LBLOCK/LRECL
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRTRK=NPRBLK*NBLKS
      NRECS=3800
      CALL ULINE (LP,2)
      WRITE (LP,70) NRECS,'HCLDEFIN'
      TRKS=1.*NRECS/NPRTRK
      NTRKS=TRKS
      IF (TRKS-NTRKS.GT.0) NTRKS=NTRKS+1
      IF (XTYPE.EQ.'GLOBAL') IUNIT=KDEFNG
      IF (XTYPE.EQ.'LOCAL') IUNIT=KDEFNL
      NDSTRK(IUNIT)=NTRKS
      CALL ULINE (LP,1)
      WRITE (LP,80) 'HCLDEFIN',IUNIT,NTRKS
      NTTRKS=NTTRKS+NTRKS
      NPTRKS(IPTRKS+1)=NTRKS
      IPTRKS=IPTRKS+1
C
C  GET DCB FOR LOCAL DEFAULTS DATASET
      CALL UFLDCB (DCBDDN,DCBMBR,'HCLLDFLT',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRBLK=LBLOCK/LRECL
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) GO TO 30
      NPRTRK=NPRBLK*NBLKS
      NRECS=2000
      CALL ULINE (LP,2)
      WRITE (LP,70) NRECS,'HCLLDFLT'
      TRKS=1.*NRECS/NPRTRK
      NTRKS=TRKS
      IF (TRKS-NTRKS.GT.0) NTRKS=NTRKS+1
      IUNIT=KLDFGD
      NDSTRK(IUNIT)=NTRKS
      CALL ULINE (LP,1)
      WRITE (LP,80) 'HCLLDFLT',IUNIT,NTRKS
      NTTRKS=NTTRKS+NTRKS
      NPTRKS(IPTRKS+1)=NTRKS
      IPTRKS=IPTRKS+1
C
      CALL ULINE (LP,2)
      WRITE (LP,90) NTTRKS
      GO TO 10
C
30    ISTAT=1
      GO TO 50
C
40    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,140) 'PASSWORD'
         GO TO 10
         ENDIF
      NFLD=2
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.4) THEN
         CALL UWARN (LP,1,-1)
         WRITE (LP,150) NCHAR
         NCHAR=4
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),PASSWD,NCHAR)
      GO TO 10
C
50    IF (ISTAT.EQ.0) THEN
      IF (XTYPE.EQ.'GLOBAL') THEN
         CALL ULINE (LPUNCH,3)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,100) XCMD,XTYPE,OPTN
         WRITE (LPUNCH,100) ' '
         WRITE (LPUNCH,100) XCMD
         LINE=' '//XTYPE
         CALL KKCONC(LINE,OPTN,DLIM)
         WRITE (LPUNCH,100) LINE(1:LENSTR(LINE))
C end of changes
      ELSE
         CALL ULINE (LPUNCH,3)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,100) XCMD,XTYPE
         WRITE (LPUNCH,100) ' '
         WRITE (LPUNCH,100) XCMD
         LINE=' '//XTYPE
         WRITE (LPUNCH,100) LINE(1:LENSTR(LINE))
C end of changes 
         ENDIF
         IF (PASSWD.EQ.'?') THEN
            CALL UWARN (LP,1,-1)
            WRITE (LP,120)
            ENDIF
         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C        WRITE (LPUNCH,110) 'PASSWORD',PASSWD
      LINE=' PASSWORD'
      CALL KKCONC(LINE,PASSWD,DLIM)
      WRITE (LPUNCH,100) LINE(1:LENSTR(LINE))
C end of changes
      
      CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C        WRITE (LPUNCH,130) 'TRACKS',(NPTRKS(I),I=1,IPTRKS)
      LINE=' TRACKS'
      DO 95 I=1,IPTRKS
         CALL KKI2AP(NPTRKS(I),CHAR,CLEN)
         CALL KKCONC(LINE,CHAR,DLIM)
 95   CONTINUE
      WRITE (LPUNCH,100) LINE(1:LENSTR(LINE))
C end of changes
      
      CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C       WRITE (LPUNCH,110) ' END'
      WRITE (LPUNCH,100) ' END'
C end of changes
      ENDIF
C     
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('+*** ERROR - INVALID FILE TYPE : ',A)
70    FORMAT ('0*** NOTE - ',I5,' RECORDS NEEDED FOR FILE ',A,'.')
C jgg following line replaced by jto with below for bug r25-12
C 80    FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I3)
80    FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I5)
90    FORMAT ('0TOTAL TRACKS = ',I5)
C jgg following lines replaced by jto with below for bug r25-12
C 100   FORMAT (/ A / A,1X,A)
C 110   FORMAT (A,1X,A)
100   FORMAT (A)
120   FORMAT ('+*** WARNING - PASSWORD NOT SPECIFIED AND ',
     *   'WILL BE SET TO ?.')
C 130   FORMAT (A,1X,3(I3,1X))
C end of changes
140   FORMAT ('+*** ERROR - INVALID NUMBER OF FIELD ON ',A,' CARD.')
150   FORMAT ('+*** WARNING - PASSWORD HAS ',I2,' CHARACTERS.  THE ',
     *        'FIRST 4 CHARACTERS WILL BE USED.')
160   FORMAT ('+*** ERROR - ',A,' IS INVALID.  MUST BE SPECIFIED AS ',
     *        'EITHER OLD OR NEW.  THIS OPTION WILL BE IGNORED.')
C
      END
