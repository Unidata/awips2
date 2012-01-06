C MEMBER ULEFTC
C-----------------------------------------------------------------------
C
      SUBROUTINE ULEFTC (STRING,NCHAR,LCHAR)
C
C  ROUTINE TO LEFT JUSTIFY CHARACTERS IN A VARIABLE.
C
C  INPUT ARGUMENTS
C     STRING - CHARACTER STRING TO BE LEFT JUSTIFIED
C     NCHAR  - NUMBER OF CHARACTERS IN STRING
C
C  OUTPUT ARGUMENTS
C     LCHAR  - LOCATION OF LAST NON-BLANK CHARACTER IN STRING
C
      CHARACTER*1 STRING(1)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uleftc.f,v $
     . $',                                                             '
     .$Id: uleftc.f,v 1.1 1995/09/17 19:02:44 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,10) NCHAR
         ENDIF
C
      IF (NCHAR.EQ.0) GO TO 90
C
C  FIND FIRST NON-BLANK CHARACTER
      DO 20 IBEG=1,NCHAR
         IF (STRING(IBEG).NE.' ') GO TO 30
20       CONTINUE
      LCHAR=NCHAR
      GO TO 90
C
C  CHECK IF FIRST NON-BLANK CHARACTER IS AT BEGINNING OF STRING
30    IF (IBEG.EQ.1) GO TO 70
C
C  MOVE CHARACTERS TO BEGINNING OF STRING
40    NUM=NCHAR-IBEG+1
      DO 50 I=1,NUM
         STRING(I)=STRING(IBEG+I-1)
50       CONTINUE
C
C  BLANK REST OF STRING
      IEND=NUM+1
      DO 60 I=IEND,NCHAR
         STRING(I)=' '
60       CONTINUE
C
C  FIND LAST NON-BLANK CHARACTER
70    LCHAR=NCHAR
      DO 80 I=1,NCHAR
         IF (STRING(LCHAR).NE.' ') GO TO 90
         LCHAR=LCHAR-1
80       CONTINUE
C
90    IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,100) LCHAR,(STRING(I),I=1,LCHAR)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER ULEFTC - NCHAR=',I2)
100   FORMAT (' *** EXIT ULEFTC - LCHAR=',I2,3X,
     *   'STRING=',100A1)
C
      END
