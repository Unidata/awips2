C MEMBER UFLDST
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT ERROR MESSAGES FOR STATUS CODES RETURNED BY ROUTINE
C  TO GET NEXT FIELD.
C
      SUBROUTINE UFLDST (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT,
     *   NUMERR,NUMWRN)
C
      DIMENSION CHAR(1)
C
      INCLUDE 'uiox'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufldst.f,v $
     . $',                                                             '
     .$Id: ufldst.f,v 1.1 1995/09/17 19:00:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTAT.EQ.0) GO TO 70
C
C  CHECK FOR VALID STATUS CODE
      IF (ISTAT.GE.1.AND.ISTAT.LE.6) GO TO 10
         CALL UWARN (LP,1,NUMWRN)
         WRITE (LP,80) ISTAT
         GO TO 70
C
C  NULL FIELD
10    IF (ISTAT.NE.1) GO TO 20
         CALL ULINE (LP,2)
         WRITE (LP,90) NFLD
         GO TO 70
C
C  NOT ENOUGH ROOM IN CHARACTER VARIABLE
20    IF (ISTAT.NE.2) GO TO 30
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,100) NFLD
         CALL ULINE (LP,1)
         WRITE (LP,110) LENGTH,IABS(LCHAR)
         GO TO 70
C
C  END OF INPUT
30    IF (ISTAT.NE.3) GO TO 40
         CALL ULINE (LP,2)
         WRITE (LP,120)
         GO TO 70
C
C  INVALID VALUE FOR REPEAT FACTOR
40    IF (ISTAT.NE.4) GO TO 50
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,130) NFLD
         GO TO 70
C
C  VALID DATE FIELD
50    IF (ISTAT.NE.5) GO TO 60
         CALL ULINE (LP,2)
         WRITE (LP,140) NFLD
         GO TO 70
C
C  ENDING QUOTE NOT FOUND FOR CHARACTER STRING
60    IF (ISTAT.NE.6) GO TO 70
         CALL UEROR (LP,1,NUMERR)
         WRITE (LP,150) NFLD
         CALL UPRCRD (LP)
         GO TO 70
C
70    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT ('+*** WARNING - IN UFLDST - STATUS CODE NOT RECOGNZED : ',
     *   I3)
90    FORMAT ('0*** NOTE - NULL FIELD FOUND IN CARD FIELD ',I2,'.')
100   FORMAT ('+*** ERROR - NOT ENOUGH ROOM IN VARIABLE TO HOLD ',
     *   'CHARACTER STRING FOUND IN CARD FIELD ',I2,'.')
110   FORMAT (T14,'LENGTH OF FIELD IS ',I2,' CHARACTERS. ',
     *   'DIMENSION OF CHARACTER STRING VARIABLE IS ',I3,'.')
120   FORMAT ('0*** NOTE - END OF INPUT ENCOUNTERED.')
130   FORMAT ('+*** ERROR - INVALID VALUE FOR REPEAT FACTOR FOUND IN ',
     *   'CARD FIELD ',I2,'.')
140   FORMAT ('0*** NOTE - VALID DATE FIELD FOUND IN CARD FIELD ',I2,
     *   '.')
150   FORMAT ('+*** ERROR - ENDING QUOTE NOT FOUND FOR CHARACTER ',
     *   'STRING IN CARD FIELD ',I2,'.')
C
      END
