C MEMBER SUPPST
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/21/94.11:01:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC ROUTINE TO SET PPINIT STATUS FLAG IN USERPARM FILE
C
      SUBROUTINE SUPPST (IPSTAT,STAT)
C
C
      INCLUDE 'uunits'
      INCLUDE 'hclcommon/huprm2'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/suppst.f,v $
     . $',                                                             '
     .$Id: suppst.f,v 1.2 1999/01/19 21:37:41 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,20)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'IPSTAT=',IPSTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK IF STATUS FLAG IS SET
      IF (IPSTAT.EQ.0) THEN
         IREC=2
         CALL UREADT (KUPARM,IREC,IFLSTA,IERR)
         IF (IERR.GT.0) THEN
            ISTAT=1
            GO TO 10
            ENDIF
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'IPPSTA=',IPPSTA
            CALL SULINE (IOSDBG,1)
            ENDIF
         IF (IPPSTA.EQ.0) GO TO 10
         ENDIF
C
C  SET STATUS FLAG
      IPPSTA=IPSTAT
      IREC=2
      CALL UWRITT (KUPARM,IREC,IFLSTA,IERR)
      IF (IERR.GT.0) ISTAT=1
      CALL UCLOST (KUPARM)
C
10    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,30)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' *** ENTER SUPPST')
30    FORMAT (' *** EXIT SUPPST')
C
      END
