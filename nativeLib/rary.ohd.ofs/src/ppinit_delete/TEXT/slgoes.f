C MODULE SLGOES
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE STATIONS FROM GOES CONTROL FILE.
C
      SUBROUTINE SLGOES (NFLD,STAID,PTYPE,GOESID,TYPMSG,ISTAT)
C
      CHARACTER*4 TYPE,PTYPE
      CHARACTER*8 STAID,GOESID,TYPMSG
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slgoes.f,v $
     . $',                                                             '
     .$Id: slgoes.f,v 1.4 2001/06/13 14:02:07 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SLGOES'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,130) NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
      NDELTE=0
C
C  CHECK IF INPUT FIELD TO BE READ
      IF (NFLD.EQ.-1) GO TO 70
      IF (NFLD.EQ.-2) GO TO 70
C
      ISTRT=-1
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NUMERR=0
      NUMWRN=0
      NXTFLD=1
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
            WRITE (IOSDBG,160) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR END OF INPUT
      IF (NFLD.EQ.-1) GO TO 80
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 80
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,170) NFLD
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
         IF (TYPE.NE.'GOES') THEN
            WRITE (LP,140) CHK(1:LENSTR(CHK))
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 80

      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,150) CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      GO TO (40,50,60),NXTFLD
C
C  SET IDENTIFIER
40    CALL SUBSTR (CHAR,1,LEN(STAID),STAID,1)
      NXTFLD=2
      GO TO 10
C
C  SET PLATFORM TYPE
50    IF (CHAR.EQ.'GHB5'.OR.
     *    CHAR.EQ.'GPLT'.OR.
     *    CHAR.EQ.'CDAS') THEN
         ELSE
            WRITE (LP,180) CHAR
            CALL SUERRS (LP,2,NUMERR)
            GO TO 10
         ENDIF
      PTYPE=CHAR
      NXTFLD=3
      GO TO 10
C
C  SET PLATFORM IDENTIFIER
60    CALL SUBSTR (CHAR,1,LEN(GOESID),GOESID,1)
      NXTFLD=1
C
70    IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      'STAID=',STAID,
     *      'GOESID=',GOESID,
     *      'PTYPE=',PTYPE,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET PLATFORM TYPE CODE
      IF (PTYPE.EQ.'GHB5') IDTYPE=0
      IF (PTYPE.EQ.'GPLT') IDTYPE=1
      IF (PTYPE.EQ.'CDAS') IDTYPE=2
C
C  CHECK IF GOES ID IS SAME AS STATION ID
      IF (GOESID.EQ.'SAME') THEN
         CALL SUBSTR (STAID,1,LEN(GOESID),GOESID,1)
         ENDIF
C
C  OPEN DATA BASE
      CALL SUDOPN (1,'GOES',IERR)
      IF (IERR.NE.0) THEN
         IF (NFLD.EQ.-2) GO TO 100
         GO TO 10
         ENDIF
C
C  DELETE STATION FROM GOES CONTROL FILE
      CALL WDGD (PUSRID,GOESID,IDTYPE,STAID,IERR)
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,200)
            CALL SUERRS (LP,2,NUMERR)
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,210) STAID,PTYPE,GOESID
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,220) TYPMSG(1:LENSTR(TYPMSG)),PTYPE,GOESID,STAID
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
            WRITE (LP,250) IERR,PTYPE,GOESID,STAID
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         IF (NFLD.EQ.-2) GO TO 100
         GO TO 10
         ENDIF
C
C  STATION SUCCESSFULLY DELETED
      CALL SUDWRT (1,'GOES',IERR)
      IF (NFLD.GT.0) THEN
         WRITE (LP,120)
         CALL SULINE (LP,1)
         ENDIF
      WRITE (LP,260) PTYPE,GOESID,STAID
      CALL SULINE (LP,1)
      NDELTE=NDELTE+1
C
      IF (NFLD.EQ.-2) GO TO 100
C      
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK NUMBER OF FIELDS FOUND
80    IF (NXTFLD.EQ.1) GO TO 90
         WRITE (LP,270) NXTFLD
         CALL SUWRNS (LP,2,NUMWRN)
C
C  PRINT NUMBER OF STATIONS DELETED
90    IF (NDELTE.EQ.0) THEN
         WRITE (LP,280)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,290) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
100   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SLGOES'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
120   FORMAT (' ')
130   FORMAT (' NFLD=',I2)
140   FORMAT ('0*** ERROR - IN SLGOES - ',A,' IS AN INVALID ',
     *   'PARAMETER TYPE.')
150   FORMAT (' INPUT FIELD = ',A)
160   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
170   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
180   FORMAT ('0*** ERROR - INVALID GOES PLATFORM TYPE : ',A)
200   FORMAT ('0*** ERROR - SYSTEM ERROR ACCESSING ',
     *   'THE GOES CONTROL FILE.')
210   FORMAT ('0*** WARMOMG - ',A,' NO STATION DEFINED IN ',
     *   'THE GOES CONTROL FILE.')
220   FORMAT ('0*** ',A,' - STATION ',A,' NOT DEFINED IN ',
     *   'THE GOES CONTROL FILE (',A,' ID=',A,').')
250   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WDGD ',
     *   'ENCOUNTERED WHILE DELETING ',A,' IDENTIFIER ',A,' FROM ',
     *   'THE GOES CONTROL FILE FOR STATION ',A,').')
260   FORMAT (' *** NOTE - ',A,' IDENTIFIER ',A,' FOR STATION ',A,
     *   ' SUCCESSFULLY DELETED FROM ',
     *   'THE GOES CONTROL FILE.')
270   FORMAT ('0*** WARNING - INPUT FIELD NUMBER ',I1,' WAS NOT FOUND.')
280   FORMAT ('0*** NOTE - NO  STATIONS SUCCESSFULLY DELETED.')
290   FORMAT ('0*** NOTE - ',I3,' STATIONS SUCCESSFULLY DELETED.')
C
      END
