C MODULE SUEND
C-----------------------------------------------------------------------
C
C  ROUTINE TO END PPINIT RUN.
C
      SUBROUTINE SUEND
C
      INCLUDE 'uiox'
      common /CMPPINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'ufreex'
      INCLUDE 'ustopx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/supagx'
      INCLUDE 'scommon/surunx'
      INCLUDE 'scommon/sutmrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/suend.f,v $
     . $',                                                             '
     .$Id: suend.f,v 1.4 2001/06/13 13:29:41 dws Exp $
     . $' /
C    ===================================================================
C

C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUEND'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
C  CHECK IF NEW PAGE TO BE STARTED
      IF (IOPNWP.EQ.1) CALL SUPAGE
C
      WRITE (LP,80)
      CALL SULINE (LP,2)
      WRITE (LP,80)
      CALL SULINE (LP,2)
      IF (ISALL.EQ.1) THEN
         WRITE (LP,70)
         CALL SULINE (LP,1)
         ENDIF
      WRITE (LP,90) PGMNAM,PGMNAM,PGMNAM
      CALL SULINE (LP,0)
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,90) PGMNAM,PGMNAM,PGMNAM
         CALL SULINE (LP,0)
         WRITE (LP,90) PGMNAM,PGMNAM,PGMNAM
         CALL SULINE (LP,0)
         ENDIF
C
C  PRINT NUMBER OF COMMANDS NOT PROCESSED
      IF (NDCCMD.GT.0) THEN
         NDCCRD=NRDCRD-NRDADD
         IF (NDCCRD.GT.0) THEN
            WRITE (LP,100) NDCCMD,NDCCRD
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  PRINT NUMBER OF COMMANDS PROCESSED
      WRITE (LP,110) NUMCMD,NRDCRD
      CALL SULINE (LP,2)
      IF (NRDADD.GT.0) THEN
         WRITE (LP,150) NRDADD
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ITMAUT.EQ.1) CALL SUTIMR (LP,ITMELA,ITMTOT)
C
C  CHECK IF COMMAND LOG OPTION SPECIFIED
      IF (IOPCLG(1).EQ.0) GO TO 40
         IUCLOG=IOPCLG(2)
         WRITE (IUCLOG,70)
         CALL SULINE (IUCLOG,1)
         WRITE (IUCLOG,120) PGMNAM
         CALL SULINE (IUCLOG,1)
         WRITE (IUCLOG,135) NPSPAG,LP
         CALL SULINE (IUCLOG,1)
         WRITE (IUCLOG,140) NPSNLR,LP
         CALL SULINE (IUCLOG,1)
         IF (ICDPUN.EQ.LP) GO TO 40
            IF (NPUCRD.GT.0) THEN
               WRITE (IUCLOG,140) NPUCRD,ICDPUN
               CALL SULINE (IUCLOG,1)
               ENDIF
C
C  PRINT TOTAL LINES WRITTEN TO PRINT OUTPUT DATA SET
40    WRITE (LP,130) NPSNLR,LP
      CALL SULINE (LP,2)
C
C  CHECK IF PUNCH OUTPUT UNIT IS SAME AS PRINT OUTPUT UNIT
      IF (ICDPUN.EQ.LP) GO TO 50
C
C  PRINT TOTAL LINES WRITTEN TO PUNCH OUTPUT DATA SET
      IF (NPUCRD.GT.0) THEN
         WRITE (LP,130) NPUCRD,ICDPUN
         CALL SULINE (LP,2)
         ENDIF
C
C  UPDATE DATA BASE FILE CONTROL INFORMATION AND CLOSE FILES
50    CALL SUDCLS (IERR)
C
C  UPDATE PPINIT STATUS FLAG
      IPPSTA=0
      CALL SUPPST (IPPSTA,IERR)
C
C  CLOSE ALL FILES
      CALL UCLOSL
C
C  UPDATE CPU TIMER
      IUNIT=-1
      CALL SUTIMR (IUNIT,ITMELA,ITMTOT)
C
C  STOP PROGRAM EXECUTION
      ISTOP=-2
      CALL SUSTOP (ISTOP)
      INCLUDE 'cluprimc'
      ISTOPX=ISTOP
      CALL USTOP2
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUEND'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' ')
80    FORMAT ('0')
90    FORMAT ('+',3('*****   ',A,' RUN COMPLETED   '),'*****')
100   FORMAT ('0*** NOTE - ',I4,'  COMMANDS AND  ',I4,'  CARD IMAGES ',
     *   'NOT DECODED.')
110   FORMAT ('0*** NOTE - ',I4,'  COMMANDS AND  ',I4,'  CARD ',
     *   'IMAGES PROCESSED.')
120   FORMAT (' *** ',A,' RUN COMPLETED ***')
130   FORMAT ('0*** NOTE - ',I6,' LINES WRITTEN TO UNIT ',I2.2,'.')
135   FORMAT (' NOTE: ',I6,' PAGES WRITTEN TO UNIT ',I2.2,'.')
140   FORMAT (' NOTE: ',I6,' LINES WRITTEN TO UNIT ',I2.2,'.')
150   FORMAT ('0*** NOTE - ',I4,'  CARD IMAGES FOUND AFTER @STOP ',
     *   'COMMAND.')
C
      END
