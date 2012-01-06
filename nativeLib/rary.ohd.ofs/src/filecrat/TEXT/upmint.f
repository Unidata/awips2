C MODULE UPMINT
C-----------------------------------------------------------------------
C
      SUBROUTINE UPMINT (USER,LDEBUG)
C
C  ROUTINE TO INITILIZE THE OFS USER PARAMETER DATASET.
C
      CHARACTER*(*) USER
      CHARACTER*8 XWORD,XWORD2
C
      INTEGER IDFLTS(60)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
      INCLUDE 'uunits'
      INCLUDE 'hclcommon/hdflts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/upmint.f,v $
     . $',                                                             '
     .$Id: upmint.f,v 1.2 1999/04/26 11:13:16 page Exp $
     . $' /
C    ===================================================================
C
C
C
      LDFLTS=60
C
      CALL ULINE (LP,2)
      WRITE (LP,160)
C
C  PRINT CARD
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
      NUMERR=0
C
      CALL UMEMST (0,TIME,3)
      CALL UMEMST (0,HNAMRF,2)
      METRIC=0
      CALL UMEMST (0,XOPEN,5)
      ZOFF=1030
      CALL SUBSTR ('E',1,1,CLKZON,1)
      CALL UMEMST (0,TDATES,6)
      LOCAL=0
      NLSTZ=0
      NHOPDB=0
      NHOCAL=0
      INTDFL=6
C
C  READ CARD
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,170) 'RPCARD',IERR
         GO TO 150
         ENDIF
      CALL UFREE (1,72)
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'IFTYPE(2)=',IFTYPE(2)
         ENDIF
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
C
      IF (XWORD.EQ.'TIME') GO TO 20
      IF (XWORD.EQ.'LOCAL') GO TO 30
      IF (XWORD.EQ.'NLSTZ') GO TO 40
      IF (XWORD.EQ.'NHOCAL') GO TO 50
      IF (XWORD.EQ.'ZOFF') GO TO 60
      IF (XWORD.EQ.'CLKZONE') GO TO 90
      IF (XWORD.EQ.'INTERVAL') GO TO 110
      IF (XWORD.EQ.'END') GO TO 140
      CALL UEROR (LP,1,NUMERR)
      WRITE (LP,180) XWORD
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET THE TIME DEFAULT
C
20    TIME(1)=0
      IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.4) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'TIME') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      IF (NCHAR.GT.4) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),TIME(3),NCHAR)
      CALL HCKDTC (TIME(3),TIME(2),IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      CALL ULINE (LP,2)
      WRITE (LP,200) XWORD,TIME(3)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF LOCAL
C
30    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.5) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'LOCAL') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),LOCAL)
      CALL ULINE (LP,2)
      WRITE (LP,220) XWORD,LOCAL
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF NLSTZ
C
40    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.5) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'NLSTZ') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NLSTZ)
      CALL ULINE (LP,2)
      WRITE (LP,220) XWORD,NLSTZ
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF NHOCAL
C
50    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.6) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'NHOCAL') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NHOCAL)
      CALL ULINE (LP,2)
      WRITE (LP,220) XWORD,NHOCAL
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF ZOFF
C
C  ZOFF IS THE OFFSET FROM ZERO Z-TIME THAT THE USER WANTS THE
C  CALANDER DATE REFERENCED BY THE '*' TO ADVANCE.
C
60    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.4) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'ZOFF') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
C
C  DETERMINE NUMBER OF DIGITS IN VALUE
C   IF 1 OR 2, ONLY THE HOUR ASSUMED SPECIFIED
C   IF 3 OR 4, BOTH THE HOUR AND MINUTE ASSUMED SPECIFIED
      INEG=1
      CALL UCMPAR ('-',IBUF(IFSTRT(NFLD)),1,INEG)
      ISTART=IFSTRT(NFLD)
      IF (INEG.EQ.0) ISTART=ISTART+1
      NDIGIT=IFSTOP(NFLD)-ISTART+1
      IF (NDIGIT.GT.4) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
C
C  DETERMINE INTEGER VALUE ENTERED
      IPOWER=4-(NDIGIT+1)/2*2
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
      INTEGR=INTEGR*10**IPOWER
      MINVAL=-200
      MAXVAL=1400
      IF (INTEGR.LT.MINVAL.OR.INTEGR.GT.MAXVAL) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,230) INTEGR,XWORD,MINVAL,MAXVAL
         GO TO 10
         ENDIF
C
      ZOFF=INTEGR
      CALL ULINE (LP,2)
      WRITE (LP,220) XWORD,ZOFF
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF CLKZONE
C
C  CLKZONE IS THE TIME ZONE CODE OF THE MACHINE CLOCK
C
90    IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.7) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'CLKZONE') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.3) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD2=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD2,NCHAR)
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,*) 'XWORD2=',XWORD2
         ENDIF
C
C  CHECK FOR VALID TIME ZONE CODE
      IF (XWORD2.EQ.'Z') GO TO 100
      IF (XWORD2.EQ.'EASTERN') GO TO 100
      IF (XWORD2.EQ.'CENTRAL') GO TO 100
      IF (XWORD2.EQ.'MOUNTAIN') GO TO 100
      IF (XWORD2.EQ.'PACIFIC') GO TO 100
      IF (XWORD2.EQ.'ALASKA') GO TO 100
      CALL UEROR (LP,1,NUMERR)
      WRITE (LP,190) XWORD2
      GO TO 10
C
100   CALL SUBSTR (XWORD2,1,1,CLKZON,1)
      CALL ULINE (LP,2)
      WRITE (LP,200) XWORD,CLKZON
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET VALUE OF INTERVAL
C
110   IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (NCHAR.NE.8) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      IF (XWORD.NE.'INTERVAL') THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
C
C  CHECK IF INTEGER VALUE
      CALL UCKINT (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,190) XWORD
         GO TO 10
         ENDIF
C
C  GET INTEGER VALUE
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),INTEGR)
      MINVAL=1
      MAXVAL=24
      IF (INTEGR.LT.MINVAL.OR.INTEGR.GT.MAXVAL) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,230) INTEGR,XWORD,MINVAL,MAXVAL
         GO TO 10
         ENDIF
      IF (MOD(24,INTEGR).NE.0) THEN
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,240) XWORD
         GO TO 10
         ENDIF
C
      INTDFL=INTEGR
      CALL ULINE (LP,2)
      WRITE (LP,220) XWORD,INTDFL
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COMPUTE VALUE OF NHOPDB
140   NHOPDB=LOCAL-NLSTZ
      CALL ULINE (LP,2)
      WRITE (LP,220) 'NHOPDB',NHOPDB
C
C  GET USER NAME
      CALL SUBSTR (USER,1,8,HNAMRF,1)
      CALL ULINE (LP,2)
      WRITE (LP,260) USER
C
      IF (NUMERR.GT.0) GO TO 150
C
      NUNIT=KUPARM
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 150
C
C  WRITE RECORD 1
      CALL UMEMST (0,IDFLTS,LDFLTS)
      CALL UMEMOV (TIME(1),IDFLTS(1),5)
      CALL UMEMOV (ZOFF,IDFLTS(7),3)
      CALL UMEMOV (LOCAL,IDFLTS(22),4)
      NREC=1
      CALL UWRITT (NUNIT,NREC,IDFLTS,IERR)
C
C  WRITE RECORD 2
      CALL UMEMST (0,IDFLTS,LDFLTS)
      NREC=2
      CALL UWRITT (NUNIT,NREC,IDFLTS,IERR)
C
      WRITE (LP,270) NUNIT,NREC
C
      WRITE (LP,280)
C
150   RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
160   FORMAT ('0*** NOTE - BEGIN USER PARAMETER FILE INITIALIZATION.')
170   FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
180   FORMAT ('+*** ERROR - INVALID COMMAND : ',A)
190   FORMAT ('+*** ERROR - INVALID INPUT ON ',A,' CARD.')
200   FORMAT ('0*** NOTE - ',A,' SET TO ',A4,'.')
220   FORMAT ('0*** NOTE - ',A,' SET TO ',I5,'.')
230   FORMAT ('+*** ERROR - ',I5,' IS AN INVALID ',A,' VALUE. ',
     *   'MINIMUM VALUE IS ',I5,' AND MAXIMUM VALUE IS ',I5,'.')
240   FORMAT ('+*** ERROR - VALUE OF ',A,
     *   ' DOES NOT DIVIDE EVENLY INTO 24.')
260   FORMAT ('0*** NOTE - USER NAME SET TO ',A,'.')
270   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED ',
     *   'WITH ',I2,' RECORDS.')
280   FORMAT ('0*** NOTE - USER PARAMETER FILE ',
     *   'SUCCESSFULLY INITIALIZED.')
C
      END
