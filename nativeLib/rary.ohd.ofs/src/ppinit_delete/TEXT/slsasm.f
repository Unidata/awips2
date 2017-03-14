C MODULE SLSASM
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE STATIONS FROM SASM CONTROL FILE.
C
      SUBROUTINE SLSASM (NFLD,STAID,TYPMSG,ISTAT)
C
      CHARACTER*4 TYPE
      CHARACTER*8 STAID,TYPMSG
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slsasm.f,v $
     . $',                                                             '
     .$Id: slsasm.f,v 1.4 2001/06/13 14:02:19 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLSASM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,110) NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NDELTE=0
C
C  CHECK IF INPUT FIELD TO BE READ     
      IF (NFLD.EQ.-1) GO TO 50   
      IF (NFLD.EQ.-2) GO TO 50
C
      ISTRT=-1
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NUMWRN=0
      IF (TYPMSG.EQ.' ') TYPMSG='WARNING'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR IDENTIFIERS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *      CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,140) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 60
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 60
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,150) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LCHK,CHK,ISTRT,1,LENGTH,IERR)
C
      NUMFLD=NUMFLD+1
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (NUMFLD.EQ.1) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
C     CHECK PARAMETER TYPE
         TYPE=CHK
         IF (TYPE.NE.'SASM') THEN
            WRITE (LP,120) CHK(1:LENSTR(CHK))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 60
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130) CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET IDENTIFIER
      STAID=CHAR
C
50    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'STAID=',STAID
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'SASM',IERR)
      IF (IERR.NE.0) THEN
         IF (NFLD.EQ.-2) GO TO 80
         GO TO 10
         ENDIF
C
C  DELETE STATION FROM SASM CONTROL FILE
      CALL WDSD (PUSRID,STAID,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,160)
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,170)
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         IF (IERR.EQ.3) THEN
            WRITE (LP,180) TYPMSG(1:LENSTR(TYPMSG)),STAID
            IF (TYPMSG.EQ.'ERROR') THEN
               CALL SUERRS (LP,2,NUMERR)
               ELSE
                  IF (TYPMSG.EQ.'WARNING') THEN
                     CALL SUWRNS (LP,2,NUMWRN)
                     ELSE
                        CALL SULINE (LP,2)
                     ENDIF
               ENDIF
            ENDIF
         IF (IERR.GT.3) THEN
            WRITE (LP,220) IERR,STAID
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         IF (NFLD.EQ.-2) GO TO 80
         GO TO 10
         ENDIF
C
C  STATION SUCCESSFULLY DELETED
      CALL SUDWRT (1,'SASM',IERR)
      IF (NFLD.GT.0) THEN
         WRITE (LP,100)
         CALL SULINE (LP,1)
         ENDIF
      WRITE (LP,240) STAID
      CALL SULINE (LP,1)
      NDELTE=NDELTE+1
C
      IF (NFLD.EQ.-2) GO TO 80
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF STATIONS DELETED
60    IF (NDELTE.EQ.0) THEN
         WRITE (LP,250)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,260) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
80    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLSASM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT (' ')
110   FORMAT (' NFLD=',I2)
120   FORMAT ('0*** ERROR  - IN SLSASM - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
130   FORMAT (' INPUT FIELD = ',A)
140   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
150   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
160   FORMAT ('0*** ERROR - SYSTEM ERROR ACCESSING ',
     *   'THE SASM CONTROL FILE.')
170   FORMAT ('0*** WARNING - NO STATION DEFINED IN ',
     *   'THE SASM CONTROL FILE.')
180   FORMAT ('0*** ',A,' - STATION ',A,' NOT FOUND IN ',
     *   'THE SASM CONTROL FILE.')
220   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WDSD ',
     *   'ENCOUNTERED WHILE DELETING STATION ',A,' FROM ',
     *   'THE SASM CONTROL FILE.')
240   FORMAT (' *** NOTE - STATION ',A,' ',
     *   'SUCCESSFULLY DELETED FROM ',
     *   'THE SASM CONTROL FILE.')
250   FORMAT ('0*** NOTE - NO STATIONS SUCCESSFULLY DELETED.')
260   FORMAT ('0*** NOTE - ',I3,' STATIONS SUCCESSFULLY DELETED.')
C
      END
