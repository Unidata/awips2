C MEMBER SWNTWK
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  DESC WRITE NTWK PARAMETER RECORD
C
      SUBROUTINE SWNTWK (IVNTWK,UNUSED,NNWFLG,INWFLG,INWDTE,
     *   LARRAY,ARRAY,DISP,ISTAT)
C
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION INWFLG(NNWFLG),INWDTE(NNWFLG)
C
      CHARACTER*4 DISP
      CHARACTER*8 BLNK8/' '/
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swntwk.f,v $
     . $',                                                             '
     .$Id: swntwk.f,v 1.1 1995/09/17 19:16:16 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('NTWK')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,90) IVNTWK,UNUSED,NNWFLG,LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=19
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,100) LARRAY,MINLEN
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      ARRAY(1)=IVNTWK+.01
C
C  STORE CURRENT DATE
      CALL UDATEI (NMO,NDA,NYR,NHRMIN,NSEC,NJUL,IERR)
      ARRAY(2)=NMO+.01
      ARRAY(3)=NDA+.01
      ARRAY(4)=NYR+.01
      ARRAY(5)=NHRMIN+.01
      DO 10 I=1,4
         INWDTE(I)=ARRAY(I+1)+.01
10       CONTINUE
C
C  STORE NUMBER OF NEWTORK INDICATORS
      ARRAY(6)=NNWFLG+.01
C
      NPOS=6
C
C  STORE UPDATE INDICATORS
      DO 20 I=1,NNWFLG
         NPOS=NPOS+1
         ARRAY(NPOS)=INWFLG(I)+.01
20       CONTINUE
C
      IF (DISP.EQ.'NEW') GO TO 30
         NPOS=NPOS+2
         GO TO 50
C
C  LAST TWO POSITIONS ARE UNUSED
30    DO 40 I=1,2
         NPOS=NPOS+1
         ARRAY(NPOS)=UNUSED
40       CONTINUE
C
C  WRITE PARAMTER RECORD TO FILE
50    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL SUDOPN (1,4HPPP ,IERR)
      IPTR=0
      CALL WPPREC (BLNK8,'NTWK',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.GT.0) THEN
         CALL SWPPST (BLNK8,'NTWK',NPOS,IPTR,IERR)
         WRITE (LP,110) IERR
         CALL SUERRS (LP,2,-1)
         ISTAT=3
         GO TO 60
         ENDIF
C
      IF (LDEBUG.GT.0) CALL SUPDMP ('NTWK','REAL',0,NPOS,ARRAY,ARRAY)
C
60    IF (ISTAT.EQ.0) THEN
         IF (DISP.EQ.'NEW') WRITE (LP,120)
         IF (DISP.EQ.'NEW') CALL SULINE (LP,2)
         IF (DISP.EQ.'OLD') WRITE (LP,130)
         IF (DISP.EQ.'OLD') CALL SULINE (LP,2)
         CALL SUDWRT (1,4HPPP ,IERR)
         ENDIF
      IF (ISTAT.GT.0) THEN
        WRITE (LP,140)
         CALL SULINE (LP,2)
         ENDIF
C
70    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SWNTWK')
90    FORMAT (' IVNTWK=',I2,3X,'UNUSED=',F7.2,3X,'NNWFLG=',I2,3X,
     *   'LARRAY=',I5)
100   FORMAT ('0*** ERROR - IN SWNTWK - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I3,3X,
     *   'NUMBER OF WORDS NEEDED=',I3)
110   FORMAT ('0*** ERROR - IN SWNTWK - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I3)
120   FORMAT ('0*** NOTE - NTWK PARAMETERS SUCCESSFULLY ',
     *   'WRITTEN.')
130   FORMAT ('0*** NOTE - NTWK PARAMETERS SUCCESSFULLY ',
     *   'UPDATED.')
140   FORMAT ('0*** NOTE - NTWK PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN.')
150   FORMAT (' *** EXIT SWNTWK')
C
      END
