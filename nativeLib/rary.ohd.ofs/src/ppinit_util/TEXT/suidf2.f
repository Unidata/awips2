C MODULE SUIDF2
C-----------------------------------------------------------------------
C
C  ROUTINE FOR PRINTING THE PAGE NUMBER ON WHICH THE PROCESSING OF
C  AN IDENTIFIER WAS STARTED.
C
      SUBROUTINE SUIDF2 (NUMID,NUMEXC,LSARAY,ISARAY,NUMERR,NUMWRN,ISTAT)
C
      DIMENSION ISARAY(LSARAY)
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suidf2.f,v $
     . $',                                                             '
     .$Id: suidf2.f,v 1.2 2001/06/13 14:05:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('UTIL')
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUIDF2'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('UTIL')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,70) NUMID,NUMERR,NUMWRN
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      IF (NUMID.LE.1) GO TO 50
C
C  PRINT IDENTIFIER AND PAGE ON WHICH PROCESSING STARTS
      NPRLIN=5
      NTIME=NUMID/NPRLIN
      IF (MOD(NUMID,NPRLIN).NE.0) NTIME=NTIME+1
      NUM1=1
      NUM2=NUMID
      NCHK=NTIME
      IF (NCHK.GT.NPSMLN) NCHK=10
C
C  SORTED BY PAGE
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,80)
      CALL SULINE (LP,2)
      DO 10 I=1,NTIME
         WRITE (LP,100) (ISARAY(N*4-3),ISARAY(N*4-2),ISARAY(N*4-1),
     *      ISARAY(N*4),N=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,80)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
10       CONTINUE
C
      IF (NUMID.LE.3) GO TO 30
C
C  SORT BY IDENTIFIER IDENTIFIER
      NWORDS=4
      NWORD1=1
      NWORD2=2
      ISPTR=0
      CALL SUSOR2 (NWORDS,NUMID,NWORD1,NWORD2,ISARAY,ISARAY,ISPTR,IERR)
      IF (ISLEFT(2+NCHK).GT.0) CALL SUPAGE
      WRITE (LP,90)
      CALL SULINE (LP,2)
      NUM1=1
      DO 20 I=1,NTIME
         WRITE (LP,100) (ISARAY(N*4-3),ISARAY(N*4-2),ISARAY(N*4-1),
     *      ISARAY(N*4),N=NUM1,NUM2,NTIME)
         CALL SULINE (LP,1)
         IF (ISNWPG(LP).EQ.1) THEN
            WRITE (LP,90)
            CALL SULINE (LP,2)
            ENDIF
         NUM1=NUM1+1
20       CONTINUE
C
30    IF (NUMERR.GT.0) THEN
         WRITE (LP,110)
         CALL SULINE (LP,2)
         ENDIF
C
      IF (NUMWRN.GT.0) THEN
         WRITE (LP,120)
         CALL SULINE (LP,2)
         ENDIF
C
      IF (NUMEXC.GT.0) THEN
         WRITE (LP,130) NUMEXC
         CALL SULINE (LP,2)
         ENDIF
C
50    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUIDF2 : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT (' NUMID=',I4,3X,'NUMERR=',I4,3X,'NUMWRN=',I4)
80    FORMAT ('0*** NOTE - IDENTIFIERS PROCESSED AND ',
     *   'PAGE NUMBER ON WHICH PROCESSING STARTS ',
     *   '(SORTED BY PAGE NUMBER):')
90    FORMAT ('0*** NOTE - IDENTIFIERS PROCESSED AND ',
     *   'PAGE NUMBER ON WHICH PROCESSING STARTS ',
     *   '(SORTED BY IDENTIFIER):')
100   FORMAT (T13,5(2A4,'-',I4,1X,A3,5X))
110    FORMAT ('0*** NOTE - ''E'' FOLLOWING PAGE NUMBER INDICATES ',
     *   'ERRORS ENCOUNTERED PROCESSING IDENTIFIER.')
120   FORMAT ('0*** NOTE - ''W'' FOLLOWING PAGE NUMBER INDICATES ',
     *   'WARNINGS ENCOUNTERED PROCESSING IDENTIFIER.')
130   FORMAT ('0*** NOTE - ',I4,' IDENTIFIERS NOT PRINTED BECAUSE ',
     *   'MAXIMUM NUMBER OF IDENTIFIERS THAT CAN BE SUMMARIZED WAS ',
     *   'EXCEEDED.')
C
      END
