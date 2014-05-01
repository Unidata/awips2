C MEMBER ESPINP
C  (from old member UXESPINP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/17/94.09:19:36 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  ROUTINE TO INITIALIZE THE NWSRFS FORECAST SYSTEM
C  EXTENDED STREAMFLOW PREDICTION SYSTEM PARAMETER FILE
C
      SUBROUTINE ESPINP (USER,DCBDDN,DCBMBR,DSKUNT,LDEBUG)
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR
      CHARACTER*8 XWORD
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'common/esprec'
      INCLUDE 'common/espfle'
      INCLUDE 'common/espseg'
      INCLUDE 'common/eunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/espinp.f,v $
     . $',                                                             '
     .$Id: espinp.f,v 1.1 1995/09/17 19:08:43 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      LENDAT=80
C
      CALL ULINE (LP,2)
      WRITE (LP,50)
C
C  PRINT CARD
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
C  GET DCB FOR PARAMETER FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'ESPPARM',LRECL,LBLOCK,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,60) 'UFLDCB',IERR
         GO TO 40
         ENDIF
C
C  GET NUMBER OF BLOCKS PER TRACK
      NPUNIT=0
      IPRINT=2
      CALL UDKBLK ('    ',NPUNIT,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,60) 'UDKBLK',IERR
         GO TO 40
         ENDIF
      NPRBLK=LBLOCK/LRECL
      NPRTRK=NPRBLK*NBLKS
C
C  READ TRACKS CARD
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,60) 'RPCARD',IERR
         GO TO 40
         ENDIF
C
C  FIND FIELDS ON CARD
      NFCOL=1
      NLCOL=72
      CALL UFREE (NFCOL,NLCOL)
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
      IF (XWORD.EQ.'END') GO TO 40
C
      IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,70)
         GO TO 40
         ENDIF
C
C  CHECK IF SECOND FIELD IS INTEGER
      NFLD=2
      IF (IFTYPE(NFLD).NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,80) NFLD
         GO TO 40
         ENDIF
      CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NTRKS)
C
C  CALCULAGE MAXIMUM NUMBER OF RECORDS IN FILE
      MXREC=NPRTRK*NTRKS
C
C  CHECK LOGICAL RECORD LENGTH
      NWORDS=LRECL/4
      IF (NWORDS.NE.LENDAT) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,90) NWORDS,LENDAT
         GO TO 40
         ENDIF
C
      NUNIT=KEPARM
C
C  PRINT DATASET ATTRIBUTES
      IPRERR=1
      CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
      IF (IERR.GT.0) GO TO 40
C
C  INITIALIZE CONTROL RECORD
      CALL UMEMST (0,ESPDAT,LENDAT)
      ESPDAT(1)=MXREC+.01
      ESPDAT(2)=2.+.01
      ESPDAT(3)=NWORDS+.01
      IREC=1
      CALL UWRITT (NUNIT,IREC,ESPDAT,IERR)
C
C  INITIALIZE REMAINING RECORDS
      CALL UMEMST (0,ESPDAT,LENDAT)
      DO 35 IREC=2,MXREC
         CALL UWRITT (NUNIT,IREC,ESPDAT,IERR)
35       CONTINUE
C
      CALL ULINE (LP,2)
      WRITE (LP,100) NUNIT,MXREC
      GO TO 10
C
40    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
50    FORMAT ('0*** NOTE - BEGIN ESP PARAMETER FILE INITIALIZATION.')
60    FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
70    FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON TRACKS CARD.')
80    FORMAT ('+*** ERROR - INTEGER VALUE EXPECTED IN FIELD ',I2,'.')
90    FORMAT ('+*** ERROR - ',I5,' IS AN INVALID RECORD LENGTH FOR ',
     *   'ESP PARAMETER FILE (MUST BE ',I4,' WORDS).')
100   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED ',
     *   'WITH ',I5,' RECORDS.')
C
      END
