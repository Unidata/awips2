C MODULE SLPPD
C-----------------------------------------------------------------------
C
C   ROUTINE TO DELETE STATIONS FROM PREPROCESSOR DATA BASE.
C
      SUBROUTINE SLPPD (NFLD,ISTAID,IDTYPE,ISTAT)
C    
      CHARACTER*4 TYPE
      CHARACTER*20 CHAR/' '/,CHK/' '/
C      
      DIMENSION ISTAID(2)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slppd.f,v $
     . $',                                                             '
     .$Id: slppd.f,v 1.2 1998/04/07 15:18:37 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,130)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,160) NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      IPDENQ=0
      NDELTE=0
C
C  CHECK IF INPUT FIELD TO BE READ
      IF (NFLD.EQ.-1) GO TO 80
      IF (NFLD.EQ.-2) GO TO 80
C
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      NUMFLD=0
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
      IF (IERR.NE.1) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,190) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 110
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 110
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,200) NFLD
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
      IF (NUMFLD.GT.1) GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK KEYWORD
      IF (NFLD.EQ.1) CALL SUPCRD
      TYPE=CHK
      IF (TYPE.NE.'PPD') THEN
         WRITE (LP,170) 'PPD',NFLD,CHK(1:LENSTR(CHK))
         CALL SUERRS (LP,2,-1)
         ENDIF
      GO TO 10
C
C  CHECK FOR KEYWORD
60    CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IRIDCK)
      IF (IRIDCK.EQ.2) GO TO 110
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'CHK=',CHK
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET IDENTIFIER
      CALL SUBSTR (CHAR,1,8,ISTAID,1)
      IDTYPE=0
C
C  ENQ PREPROCESSOR DATA BASE
80    CALL PDENDQ ('ENQ',IERR)
      IF (IERR.EQ.0) THEN
         CALL SULINE (LP,1)
         IPDENQ=1
         ELSE
            WRITE (LP,205)
            CALL SUERRS (LP,2,-1)
            ISTAT=1
            GO TO 120
         ENDIF
C
C  OPEN PREPROCESSOR DATA BASE
      CALL SUDOPN (1,'PPD ',IERR)
      IF (IERR.GT.0) GO TO 100
C
C  DELETE STATION FROM PREPROCESSOR DATA BASE
      CALL WPDD (ISTAID,IDTYPE,IERR)
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.1) THEN
            IF (IDTYPE.EQ.0) THEN
               WRITE (LP,210) ISTAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IDTYPE.EQ.1) THEN
               WRITE (LP,220) ISTAID(1)
               CALL SUWRNS (LP,2,-1)
               ENDIF
            ENDIF
         IF (IERR.EQ.2) THEN
            IF (IDTYPE.EQ.0) THEN
               WRITE (LP,230) ISTAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IDTYPE.EQ.1) THEN
               WRITE (LP,240) ISTAID(1)
               CALL SUWRNS (LP,2,-1)
               ENDIF
            ENDIF
         IF (IERR.EQ.3) THEN
            IF (IDTYPE.EQ.0) THEN
               WRITE (LP,250) ISTAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IDTYPE.EQ.1) THEN
               WRITE (LP,260) ISTAID(1)
               CALL SUWRNS (LP,2,-1)
               ENDIF
            ENDIF
         IF (IERR.GT.3) THEN
            IF (IDTYPE.EQ.0) THEN
               WRITE (LP,270) ISTAID
               CALL SUWRNS (LP,2,-1)
               ENDIF
            IF (IDTYPE.EQ.1) THEN
               WRITE (LP,280) ISTAID(1)
               CALL SUWRNS (LP,2,-1)
               ENDIF
            ENDIF
         IF (NFLD.GT.0) GO TO 10
            ISTAT=IERR
            GO TO 120
         ENDIF
C
C  STATION SUCCESSFULLY DELETED
      CALL SUDWRT (1,'PPD ',IERR)
      IF (NFLD.GT.0) THEN
         WRITE (LP,150)
         CALL SULINE (LP,1)
         ENDIF
      WRITE (LP,290) ISTAID
      CALL SULINE (LP,1)
      IF (NFLD.GT.0) NDELTE=NDELTE+1
C
100   IF (NFLD.EQ.-2) GO TO 120
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF STATIONS DELETED
110   IF (NDELTE.EQ.0) THEN
         WRITE (LP,300)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,310) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
C  DEQ PREPROCESSOR DATA BASE
120   IF (IPDENQ.EQ.1) THEN
         CALL PDENDQ ('DEQ',IERR)
         IF (IERR.EQ.0) CALL SULINE (LP,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,320)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
130   FORMAT (' *** ENTER SLPPD')
150   FORMAT (' ')
160   FORMAT (' NFLD=',I2)
170   FORMAT ('0*** ERROR - IN SLPPD - THE CHARACTERS ''',A4,
     *   ''' WERE EXPECTED IN FIELD ',I2,' BUT ''',A4,''' WERE FOUND.')
190   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
200   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
205   FORMAT ('0*** ERROR - IN SFPDCR - ENQUING PREPROCESSOR DATA ',
     *   'BASE.')
210   FORMAT ('0*** WARNING - STATION ',2A4,' NOT DEFINED IN ',
     *   'PREPROCESSOR DATA BASE.')
220   FORMAT ('0*** WARNING - STATION NUMBER ',I5,' NOT DEFINED IN ',
     *   'PREPROCESSOR DATA BASE.')
230   FORMAT ('0*** WARNING - SIF RECORD FOR STATION ',2A4,
     *   'INDICATES STATION NUMBER IS DEFINED BUT NUMBER NOT FOUND ',
     *   'IN INDEX.')
240   FORMAT ('0*** WARNING - SIF RECORD FOR STATION ',I5,
     *   'INDICATES STATION NUMBER IS DEFINED BUT NUMBER NOT FOUND ',
     *   'IN INDEX.')
250   FORMAT ('0*** WARNING - SYSTEM ERROR ACCESSING STATION ',2A4,
     *   ' IN PREPROCESSOR DATA BASE.')
260   FORMAT ('0*** WARNING - SYSTEM ERROR ACCESSING STATION NUMBER ',
     *   I5,' IN PREPROCESSOR DATA BASE.')
270   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WPDD ',
     *   'ENCOUNTERED WHILE DELETING STATION ',2A4,
     *   ' FROM PREPROCESSOR DATA BASE.')
280   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WPDD ',
     *   'ENCOUNTERED WHILE DELETING STATION ',I5,
     *   ' FROM PREPROCESSOR DATA BASE.')
290   FORMAT (' *** NOTE - STATION ',2A4,' SUCCESSFULLY DELETED ',
     *   'FROM PREPROCESSOR DATA BASE.')
300   FORMAT ('0*** NOTE - NO  STATIONS SUCCESSFULLY DELETED.')
310   FORMAT ('0*** NOTE - ',I3,' STATIONS ',
     *   'SUCCESSFULLY DELETED.')
320   FORMAT (' *** EXIT SLPPD')
C
      END
