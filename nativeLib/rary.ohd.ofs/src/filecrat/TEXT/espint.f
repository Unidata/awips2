C MEMBER ESPINT
C  (from old member UXESPINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 08/17/94.09:19:41 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  ROUTINE TO INITIALIZE THE NWSRFS FORECAST SYSTEM
C  EXTENDED STREAMFLOW PREDICTION SYSTEM TIME SERIES FILES
C
      SUBROUTINE ESPINT (USER,DCBDDN,DCBMBR,DSKUNT,LDEBUG)
C
      CHARACTER*(*) USER,DCBDDN,DCBMBR
      CHARACTER*8 XWORD
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'common/etsdat'
      INCLUDE 'common/eunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/espint.f,v $
     . $',                                                             '
     .$Id: espint.f,v 1.1 1995/09/17 19:08:44 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      LTSDAT=124
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (LP,60)
         ENDIF
C
      CALL ULINE (LP,2)
      WRITE (LP,70)
C
C  PRINT CARD
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
C
C  GET DCB FOR TIME SERIES FILE
      CALL UFLDCB (DCBDDN,DCBMBR,'ESPTS',LRECL,LBLOCK,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,80) 'UFLDCB',IERR
         GO TO 50
         ENDIF
C
C  GET NUMBER OF BLOCKS PER TRACK
      IPRINT=2
      CALL UDKBLK (' ',0,DSKUNT,LBLOCK,IPRINT,NBLKS,IPCT,IERR)
      IF (IERR.NE.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,80) 'UDKBLK',IERR
         GO TO 50
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
         WRITE (LP,80) 'RPCARD',IERR
         GO TO 50
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
      IF (XWORD.EQ.'END') GO TO 50
C
      IF (NFIELD.GT.6) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,90)
         GO TO 50
         ENDIF
C
C  CHECK LOGICAL RECORD LENGTH
      NWORDS=LRECL/4
      IF (NWORDS.NE.LTSDAT) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,110) NWORDS,LTSDAT
         GO TO 50
         ENDIF
C
C  PROCESS EACH FILE
      DO 40 NFLD=2,NFIELD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,100) NFLD
            GO TO 40
            ENDIF
         CALL UNUMIC (IBUF,IFSTRT(NFLD),IFSTOP(NFLD),NTRKS)
         IF (NTRKS.EQ.0) GO TO 40
         MXREC=NPRTRK*NTRKS
         NUNIT=KEPERM(NFLD-1)
C     PRINT DATASET ATTRIBUTES
         IPRERR=1
         CALL UPRDSA ('NONE',NUNIT,'NONE',IPRERR,LP,IERR)
         IF (IERR.GT.0) GO TO 40
         IF (LDEBUG.GT.0) THEN
            CALL ULINE (LP,1)
            WRITE (LP,*) 'NTRKS=',NTRKS,
     *         ' LRECL=',LRECL,
     *         ' MXREC=',MXREC,
     *         ' NUNIT=',NUNIT
            ENDIF
C     INITIALIZE CONTROL RECORD
         CALL UMEMST (0,TSDAT,LTSDAT)
         TSDAT(1)=MXREC+.01
         TSDAT(2)=NWORDS+.01
         TSDAT(3)=2.+.01
         TSDAT(4)=4.+.01
         IREC=1
         CALL UWRITT (NUNIT,IREC,TSDAT,IERR)
C     INITIALIZE REMAINING RECORD
         CALL UMEMST (0,TSDAT,LTSDAT)
         DO 35 IREC=2,MXREC
            CALL UWRITT (NUNIT,IREC,TSDAT,IERR)
35          CONTINUE
         CALL ULINE (LP,2)
         WRITE (LP,120) NUNIT,MXREC
40       CONTINUE
         GO TO 10
C
50    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT (' *** ENTER ESPINT')
70    FORMAT ('0*** NOTE - BEGIN ESP TIME SERIES FILE INITIALIZATION.')
80    FORMAT ('+*** ERROR - ROUTINE ',A,' NOT SUCCESSFULLY CALLED. ',
     *   'STATUS CODE=',I2)
90    FORMAT ('+*** ERROR - INVALID NUMBER OF FIELDS ON TRACKS CARD.')
100   FORMAT ('+*** ERROR - INTEGER VALUE EXPECTED IN FIELD ',I2,'.')
110   FORMAT ('+*** ERROR - ',I5,' IS AN INVALID RECORD LENGTH FOR ',
     *   'ESP TIME SERIES FILE (MUST BE ',I4,' WORDS).')
120   FORMAT ('0*** NOTE - UNIT ',I2.2,' SUCCESSFULLY INITIALIZED ',
     *   'WITH ',I5,' RECORDS.')
C
      END
