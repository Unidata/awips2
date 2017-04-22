C MEMBER PRDINT
C  (from old member UXPRDINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/20/94.15:05:19 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE PRDINT (USER,DCBDDN,DCBMBR,DSKUNT,LDEBUG)
C
C          ROUTINE: PRDINT
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C  ROUTINE TO INITIALIZE PROCESSED DATA BASE FILES.
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR
      CHARACTER*8 XWORD
C
      DIMENSION ARR(16),IXREC(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/prdint.f,v $
     . $',                                                             '
     .$Id: prdint.f,v 1.3 1997/04/06 13:06:56 page Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C***********************************************************************
C
C
      INDERR=0
C
      CALL ULINE (LP,2)
      WRITE (LP,150)
C
C  PRINT CARD
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
C  SET USER NAME
      CALL SUBSTR (USER,1,8,USERPR,1)
C
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ MINDAY VALUE
10    CALL RPCARD (IBUF,IERR)
      CALL WPCARD (IBUF)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'RPCARD',IERR
         INDERR=1
         GO TO 20
         ENDIF
      CALL UFREE (1,72)
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
      IF (XWORD.EQ.'END') GOTO 50
C
      IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,210) 'MINDAY'
         INDERR=1
         GO TO 20
         ENDIF
      IF (NCHAR.NE.6) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'MINDAY'
         INDERR=1
         GO TO 20
         ENDIF
      IF (XWORD.NE.'MINDAY') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'MINDAY'
         INDERR=1
         GO TO 20
         ENDIF
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,220) 'MINDAY'
         INDERR=1
         GO TO 20
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),MINDAY)
      CALL ULINE (LP,2)
      WRITE (LP,170) MINDAY
      IF (MINDAY.GE.0.AND.MINDAY.LE.99) THEN
         ELSE
            CALL UWARN (LP,1,-1)
            WRITE (LP,180) MINDAY
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ NUMBER OF TRACKS FOR EACH FILE
20    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'RPCARD',IERR
         INDERR=1
         GO TO 40
         ENDIF
      CALL UFREE (1,72)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 20
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 20
C
      IF (NFIELD.NE.NUMFIL+1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,210) 'TRACKS'
         INDERR=1
         GO TO 50
         ENDIF
      IF (NCHAR.NE.6) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'TRACKS'
         INDERR=1
         GO TO 50
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
      IF (XWORD.NE.'TRACKS') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'TRACKS'
         INDERR=1
         GO TO 50
         ENDIF
C
C  GET DCB FOR DATA FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PRDTS',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'UFLDCB',IERR
         INDERR=1
         GO TO 50
         ENDIF
C
C  SET TIME SERIES DATA FILE LOGICAL RECORD LENGTH
      LRECTS=LRECL/4
C
C  CALCULATE NUMBER OF RECORDS PER TRACK BASED ON FILE UNIT
      LPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,LPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'UDKBLK',IERR
         INDERR=1
         GO TO 50
         ENDIF
      NPRBLK=LBLOCK/LRECL
      NPRTRK=NPRBLK*NBLKS
C
      DO 30 I=1,NUMFIL
         NFLD=NFLD+1
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,220) 'TRACKS'
            INDERR=1
            GO TO 30
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NTRKS)
         CALL ULINE (LP,2)
         WRITE (LP,190) KPRDTU(I),NTRKS
         MAXREC(I)=NTRKS*NPRTRK
30       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  READ NUMBER OF TRACKS FOR INDEX
40    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'RPCARD',IERR
         INDERR=1
         GO TO 50
         ENDIF
      CALL UFREE (1,72)
C
C  CHECK FOR BLANK CARD
      IF (NFIELD.EQ.0) GO TO 40
C
C  GET FIRST FIELD
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 40
C
      IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,210) 'INDEX'
         INDERR=1
         GO TO 50
         ENDIF
      IF (NCHAR.NE.5) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'INDEX'
         INDERR=1
         GO TO 50
         ENDIF
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
      IF (XWORD.NE.'INDEX') THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,200) 'INDEX'
         INDERR=1
         GO TO 50
         ENDIF
C
C  SET NUMBER OF TRACKS FOR INDEX
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,220) 'INDEX'
         INDERR=1
         GO TO 50
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NTRKI)
      CALL ULINE (LP,2)
      WRITE (LP,190) KINDEX,NTRKI
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ERRORS ENCOUNTERED
50    IF (INDERR.EQ.1) GO TO 140
C
C  INITIALIZE PARAMETER FILE
      NPRDTP=18
      MAXDTP=100
      CALL UMEMST (0,DATFIL,NPRDTP*MAXDTP)
      NUMTMS=0
      NUMDTP=0
      MAXDAT=300
      CALL UMEMST (0,MAXDUM,6)
      CALL UMEMST (0,NUMDUM,46)
      NUNIT=KRFCPR
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 60
C
      CALL ULINE (LP,2)
      WRITE (LP,230) NUNIT
C
60    IF (NTRKI.EQ.0) GO TO 90
C
C  GET DCB FOR INDEX FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'PRDINDEX',LRECL,LBLOCK,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'UFLDCB',IERR
         GO TO 140
         ENDIF
C
C  CALCULATE NUMBER OF RECORDS PER TRACK BASED ON FILE UNIT
      LPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,LPRINT,NBLKS,IPCT,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,160) 'UDKBLK',IERR
         GO TO 140
         ENDIF
      NPRBLK=LBLOCK/LRECL
      NPRTRK=NPRBLK*NBLKS
C
      NUNIT=KINDEX
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 90
C
C  INITIALIZE INDEX
      CALL UMEMST (0,IXREC,4)
      NUMREC=MAXTMS*2
      IF (NTRKI.EQ.0) GO TO 70
         NUMREC=NTRKI*NPRTRK
         MAXTMS=NUMREC/2
70    DO 80 I=1,NUMREC
         CALL UWRITT (NUNIT,I,IXREC,IERR)
         IF (IERR.NE.0) GO TO 130
80       CONTINUE
      CALL ULINE (LP,2)
      WRITE (LP,240) NUNIT,NUMREC
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  INITIALIZE CONTROL RECORD FOR TIME SERIES DATA FILES
C
90    CALL UMEMST (0,TSCNTR,80)
      TSCNTR(1,1)=KPRDTU(1)
      TSCNTR(2,1)=MAXREC(1)
      TSCNTR(3,1)=2
      TSCNTR(1,2)=KPRDTU(2)
      TSCNTR(2,2)=MAXREC(2)
      TSCNTR(3,2)=2
      TSCNTR(1,3)=KPRDTU(3)
      TSCNTR(2,3)=MAXREC(3)
      TSCNTR(3,3)=2
      TSCNTR(1,4)=KPRDTU(4)
      TSCNTR(2,4)=MAXREC(4)
      TSCNTR(3,4)=2
      TSCNTR(1,5)=KPRDTU(5)
      TSCNTR(2,5)=MAXREC(5)
      TSCNTR(3,5)=2
C
C  WRITE CONTROL RECORDS TO DATA FILES
      CALL WPDBCO (IERR)
      IF (IERR.NE.0) GO TO 130
C
      DO 100 I=1,LRECTS
         ARR(I)=ZAPR
100      CONTINUE
C
C  INITIALIZE TIME SERIES DATA FILES
      DO 120 I=1,NMPRDF
         IF (MAXREC(I).EQ.0) GO TO 120
         NUNIT=KPRDTU(I)
C     PRINT DATASET ATTRIBUTES
         IPRERR=1
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) GO TO 120
         NUMREC=MAXREC(I)
         DO 110 J=2,NUMREC
            CALL WVLRCD (NUNIT,J,1,ARR,LRECTS,IERR)
            IF (IERR.NE.0) GO TO 130
110         CONTINUE
         CALL ULINE (LP,2)
         WRITE (LP,240) NUNIT,MAXREC(I)
120      CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,250)
      GO TO 140
C
C  DAIO ERROR
130   CALL UEROR (LP,1,-1)
      WRITE (LP,260)
C
140   RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
150   FORMAT ('0*** NOTE - BEGIN PROCESSED DATA BASE FILE ',
     *   'INITIALIZATION.')
160   FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
170   FORMAT ('0*** NOTE - MINDAY SET TO ',I3,'.')
180   FORMAT ('+*** WARNING - MINDAY IS SET TO ',I4,'.')
190   FORMAT ('0*** NOTE - UNIT ',I2.2,' SET TO ',I4,' TRACKS.')
200   FORMAT ('+*** ERROR - INVALID KEYWORD ON ',A,' CARD.')
210   FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON ',A,' CARD.')
220   FORMAT ('+*** ERROR - INVALID FIELD TYPE ON ',A,' CARD.')
230   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED.')
240   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED ',
     *   'WITH ',I6,' RECORDS.')
250   FORMAT ('0*** NOTE - PROCESSED DATA BASE FILES ',
     *   'SUCCESSFULLY INITIALIZED.')
260   FORMAT ('+*** ERROR - DAIO WRITE ERROR.')
C
      END
