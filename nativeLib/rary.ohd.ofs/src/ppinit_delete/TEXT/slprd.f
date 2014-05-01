C MODULE SLPRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO DELETE TIME SERIES FROM PROCESSED DATA BASE.
C
      SUBROUTINE SLPRD (NFLD,TSID,TSTYP,ISTAT)
C
      CHARACTER*4 TYPE,TSTYP,TSTYP2
      CHARACTER*8 TSID
      CHARACTER*20 CHAR/' '/,CHK/' '/
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_delete/RCS/slprd.f,v $
     . $',                                                             '
     .$Id: slprd.f,v 1.4 1999/04/26 11:38:35 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,180)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DELT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NFLD=',NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LCHAR=LEN(CHAR)/4
      LCHK=LEN(CHK)/4
C
      NDELTE=0
C
C  CHECK OPTION TO PRINT BLANK LINE BEFORE DELETE MESSAGE
      IPRSPC=1
      IF (NFLD.EQ.-2) IPRSPC=0
C
C  CHECK IF INPUT FIELD TO BE READ
      IF (NFLD.EQ.-1) GO TO 100
      IF (NFLD.EQ.-2) GO TO 100
C
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      NUMFLD=0
      NXTFLD=1
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
            WRITE (IOSDBG,230) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
C
C  CHECK FOR END OF INPUT
20    IF (NFLD.EQ.-1) GO TO 160
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 160
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,240) NFLD
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
      IF (NUMFLD.GT.1) GO TO 60
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIRST FIELD
      TYPE=CHK
      IF (TYPE.NE.'PRD') THEN
         IF (NFLD.EQ.1) CALL SUPCRD
         WRITE (LP,210) 'PRD',NFLD,CHK(1:LENSTR(CHK))
         CALL SUERRS (LP,2,-1)
         ENDIF
      GO TO 10
C
60    IF (NXTFLD.EQ.2) GO TO 70
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DELT',CHK,NFLD,0,IKEYWD,IERR)
      IF (IERR.EQ.2) GO TO 160
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'CHK=',CHK
            CALL SULINE (IOSDBG,1)
            ENDIF
C
70    IF (NFLD.EQ.1) CALL SUPCRD
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      GO TO (80,90),NXTFLD
C
C  SET IDENTIFIER
80    TSID=CHAR
      NXTFLD=2
      GO TO 10
C
C  SET TIME SERIES TYPE
90    TSTYP=CHAR
C
C  OPEN PROCESSED DATA BASE
100   CALL SUDOPN (1,'PRD ',IERR)
      IF (IERR.NE.0) GO TO 150
C
C  DELETE TIME SERIES FROM PROCESSED DATA BASE
      TSTYP2=TSTYP
      ICKREF=0
      IPRERR=0
      IF (TSTYP2.EQ.'FMAP') THEN
         TSTYP2='MAP '
         IFUT=1
         CALL WPRDEL (TSID,TSTYP2,IFUT,ICKREF,IPRERR,IERR)
         ELSE
            IFUT=0
            CALL WPRDEL (TSID,TSTYP2,IFUT,ICKREF,IPRERR,IERR)
         ENDIF
      IF (IERR.NE.0) THEN
         IF (IERR.EQ.1) THEN
            WRITE (LP,250) TSTYP,TSID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.EQ.2) THEN
            WRITE (LP,252) TSTYP,TSID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.EQ.3) THEN
            WRITE (LP,254) TSTYP,TSID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         IF (IERR.GT.3) THEN
            WRITE (LP,260) IERR,TSTYP,TSID
            CALL SUWRNS (LP,2,-1)
            ENDIF
         GO TO 150
      ENDIF
C
C  TIME SERIES SUCCESSFULLY DELETED
      CALL SUDWRT (1,'PRD ',IERR)
      IF (IPRSPC.EQ.1) THEN
         WRITE (LP,190)
         CALL SULINE (LP,1)
         ENDIF
      WRITE (LP,270) TSTYP,TSID
      CALL SULINE (LP,1)
      IF (NFLD.GT.0) NDELTE=NDELTE+1
C
150   IF (NFLD.EQ.-2) GO TO 170
C
      NXTFLD=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT NUMBER OF TIME SERIES DELETED
160   IF (NDELTE.EQ.0) THEN
         WRITE (LP,280)
         CALL SULINE (LP,2)
         ENDIF
      IF (NDELTE.GT.0) THEN
         WRITE (LP,290) NDELTE
         CALL SULINE (LP,2)
         ENDIF
C
170   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,300)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
180   FORMAT (' *** ENTER SLPRD')
190   FORMAT (' ')
210   FORMAT ('0*** ERROR - IN SLPRD - THE CHARACTERS ''',A,
     *   ''' WERE EXPECTED IN FIELD ',I2,''' BUT ''',A,' WERE FOUND.')
230   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
240   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',I2,'.')
250   FORMAT ('0*** WARNING - ',A,' TIME SERIES NOT FOUND FOR ',
     *   'IDENTIFIER ',A,'.')
252   FORMAT ('0*** WARNING - ',A,' TIME SERIES FOR IDENTIFIER ',A,
     *   'IS REFERENCED.')
254   FORMAT ('0*** WARNING - DATA TYPE ',A,' SPECIFIED FOR ',
     *   A,' IS NOT DEFINED IN THE PROCESSED DATA BASE.')
260   FORMAT ('0*** WARNING - STATUS CODE ',I3,' FROM WPRDEL ',
     *   'ENCOUNTERED WHILE DELETING ',A,
     *   'TIME SERIES FOR IDENTIFIER ',A,'.')
270   FORMAT (' *** NOTE - ',A,' TIME SERIES FOR IDENTIFIER ',A,
     *   ' SUCCESSFULLY DELETED.')
280   FORMAT ('0*** NOTE - NO  TIME SERIES SUCCESSFULLY DELETED.')
290   FORMAT ('0*** NOTE - ',I3,' TIME SERIES ',
     *   'SUCCESSFULLY DELETED.')
300   FORMAT (' *** EXIT SLPRD')
C
      END
