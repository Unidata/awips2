C MODULE SASMDB
C-----------------------------------------------------------------------
C
C  MAIN ROUTINE FOR PROGRAM SASMDB.
C
      SUBROUTINE SASMDB_MAIN
C
      CHARACTER*8  USERID,STAID
      CHARACTER*80 LINE,WORD
      CHARACTER*128 FILENAME
C
      INCLUDE 'uiox'
      INCLUDE 'dscommon/dsunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/sasmdb/RCS/sasmdb.f,v $
     . $',                                                             '
     .$Id: sasmdb.f,v 1.6 2004/07/12 13:53:57 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
      CALL UPRIMO_SGDB (LPX,ICOND)
      IF (ICOND .NE. 0) STOP 16
C
C  PRINT PAGE HEADER
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (LP)
      CALL USETO1 ('NOPAGHDR',IERR)
C
10    IF (LPX.GT.0) WRITE (LPX,20)
20    FORMAT ('Enter one of the following commands:' /
     *        '  INIT ..... Initialize file' /
     *        '  LIST ..... List file' /
     *        '  STOP ..... End program'
     *        )
C
      KDSRCF=47
      LINE = 'Command: '
C
30    CALL UDNXWD ('N',LPX,ICD,LINE,WORD)
      CALL KKCAPS (WORD)
      IF (WORD.EQ.' '   ) GO TO 10
      IF (WORD.EQ.'INIT') GO TO 60
      IF (WORD.EQ.'LIST') GO TO 110
      IF (WORD.EQ.'STOP') GO TO 130
      IF (WORD.EQ.'Q'   ) GO TO 130
      IF (WORD.EQ.'QUIT') GO TO 130
      IF (WORD.EQ.'HELP') GO TO 10
      IF (LPX.GT.0) WRITE (LPX,50)
50    FORMAT (' *** ERROR - Invalid command.')
      GO TO 10
C
C  INITIALIZE FILE
60    IOPEN=1
      CALL UDOPEN (KDSRCF,IOPEN,FILENAME,LRECL,ISTAT)
      IF (ISTAT.NE.0) THEN
         WRITE (LP,70) FILENAME
70    FORMAT (' *** ERROR - Cannot open file ',A)
         GO TO 130
         ENDIF
      NUM=0
      WRITE (KDSRCF,REC=1,ERR=90) NUM
      WRITE (LP,80) FILENAME
80    FORMAT (' INITIALIZED FILE ',A)
      GO TO 30
90    WRITE (LP,100) FILENAME
100   FORMAT (' *** ERROR - Cannot initialize file ',A)
      GO TO 30
C
C  LIST CONTROL FILE
110   USERID=' '
      STAID=' '
      CALL DSPRT (USERID,STAID,IERR)
      IF (IERR.NE.0) WRITE (LP,120) IERR
120   FORMAT (' DSPRT STATUS CODE = ',I2)
      GO TO 30
C
C  STOP PROGRAM
130   CALL UPCLOS (KDSRCF,' ',IERR)
      CALL UPCLOS (ICD,'  ',IERR)
      CALL UPCLOS (LP, '  ',IERR)
C
      STOP
      END
C
C-----------------------------------------------------------------------
C
C      SUBROUTINE SULINE (NUNIT,NLINES)
C		CALL ULINE (NUNIT,NLINES)
C      RETURN
C      END
