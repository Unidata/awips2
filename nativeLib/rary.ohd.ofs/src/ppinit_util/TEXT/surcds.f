C MODULE SURCDS
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ INPUT CARDS, CHECK FOR @INCLUDE AND WRITE TO FILE.
C
      SUBROUTINE SURCDS (ISTAT)
C
      CHARACTER*8 DDN,TYPMSG
      CHARACTER*8 XFT/'FTXXF001'/
      CHARACTER*8 SEQNUM/' '/
      CHARACTER*8 XINCL/'@INCLUDE'/
      CHARACTER*11 XNINCL/'@NOINCLUDES'/
      CHARACTER*80 RECOLD,RECNEW
C
      INCLUDE 'uiox'
      INCLUDE 'udsatx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/suerrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/surcds.f,v $
     . $',                                                             '
     .$Id: surcds.f,v 1.5 1999/07/07 11:20:47 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,110)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C
      ICKINC=1
C
      IUCLOG=LP
C
C  CHECK IF TEMPORARY FILE ALLOCATED
      DDN=XFT
      CALL UFXDDN (DDN,ICDTMP,IERR)
      IF (IERR.EQ.0) GO TO 10
         WRITE (LP,140) IERR
         CALL SULINE (LP,1)
         GO TO 20
10    IPRERR=0
      CALL UDDST (DDN,IPRERR,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'DDN=',DDN,
     *      ' IERR=',IERR,
     *      ' IPRERR=',IPRERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IERR.EQ.0) GO TO 30
20       ISTAT=1
         WRITE (LP,150) DDN
         CALL SUWRNS (LP,2,-1)
         GO TO 100
C
30    REWIND ICDTMP
C
C  READ RECORD
40    READ (ICD,120,END=90) RECOLD
C
C  CHECK FOR COMMENT FIELD
      CALL UINDEX (RECOLD,LEN(RECOLD),'$',1,ICOLCM)
C
C  CHECK FOR @INCLUDE COMMAND
      CALL UINDEX (RECOLD,LEN(RECOLD),XINCL,LEN(XINCL),ICOLIN)
      IF (ICOLIN.GT.0) THEN
         IF (ICOLCM.EQ.0) GO TO 60
         IF (ICOLCM.GT.ICOLIN) GO TO 60
         GO TO 50
         ENDIF
C
C  CHECK FOR @NOINCLUDE COMMAND
      CALL UINDEX (RECOLD,LEN(RECOLD),XNINCL,LEN(XNINCL),ICOLIN)
      IF (ICOLIN.GT.0) THEN
         IF (ICOLCM.EQ.0) ICKINC=0
         IF (ICOLCM.GT.ICOLIN) ICKINC=0
         GO TO 40
         ENDIF
C
C  WRITE TO TEMPORARY FILE
50    WRITE (ICDTMP,120) RECOLD
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' ICDTMP=',ICDTMP,
     *      ' RECOLD=',RECOLD(1:LENSTR(RECOLD)),
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    IF (ICKINC.EQ.0) GO TO 40
C
C  EXPAND INCLUDE
C
      NSTLVL=0
C
C  CHECK FOR 'INCLUDE' STATEMENT
70    ICKCOL=0
      IPRERR=1
      TYPMSG='ERROR'
      CALL UINCLD (XINCL,RECOLD,ICKCOL,NSTLVL,RECNEW,LRECNEW,
     *   IPRERR,TYPMSG,LP,IERR)
      IF (IERR.GT.0) GO TO 80
C
C  CHECK IF END OF INPUT FROM 'INCLUDE' STATEMENT
      IF (NSTLVL.EQ.-1) GO TO 80
C
      IF (LDEBUG.GT.0) THEN
         CALL ULINE (IOSDBG,1)
         WRITE (IOSDBG,*)
     *      ' NSTLVL=',NSTLVL,
     *      ' RECNEW=',RECNEW,
     *      ' '
         ENDIF
C
C  CHECK IF ANY 'INCLUDE' STATEMENTS FOUND
      IF (NSTLVL.GT.0) THEN
         WRITE (ICDTMP,120) RECNEW
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*)
     *         ' ICDTMP=',ICDTMP,
     *         ' RECNEW=',RECNEW(1:LENSTR(RECNEW)),
     *         ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
         RECOLD=RECNEW
         GO TO 70
         ENDIF
C
80    IF (IOPCLG(1).EQ.1) THEN
         WRITE (IUCLOG,130) XINCL,SEQNUM,ICMCDE
         CALL SULINE (IUCLOG,1)
         ENDIF
C
      GO TO 40
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  RESET UNIT FROM WHICH RECORDS WILL BE READ
90    ICD=ICDTMP
      REWIND ICD
C
100   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,160)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT (' *** ENTER SURCDS')
120   FORMAT (A80)
130   FORMAT (' COMMAND ',A,' FOUND AT INPUT LINE ',A,' EXECUTED. ',
     *   'CONDITION CODE = ',I2)
140   FORMAT (' UFXDDN STATUS CODE=',I3)
150   FORMAT ('0*** WARNING - TEMPORARY FILE FOR INPUT CARD ',
     *   'IS NOT ALLOCATED. DDNAME ',A,' IS MISSING. INCLUDES WILL ',
     *   'NOT BE EXPANDED.')
160   FORMAT (' *** EXIT SURCDS')
C
      END
