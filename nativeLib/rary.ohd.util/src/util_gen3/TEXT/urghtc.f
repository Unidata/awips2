C MODULE URGHTC
C-----------------------------------------------------------------------
C
      SUBROUTINE URGHTC (STRING,NCHAR,LBEGIN)
C
C  ROUTINE TO RIGHT JUSTIFY CHARACTERS IN A VARIABLE.
C
C  INPUT ARGUMENTS
C     STRING - CHARACTER STRING TO BE RIGHT JUSTIFIED
C     NCHAR  - NUMBER OF CHARACTERS IN STRING
C
C  OUTPUT ARGUMENTS
C     LBEGIN  - LOCATION OF FIRST NON-BLANK CHARACTER IN STRING
C
      CHARACTER*1 STRING(NCHAR)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/urghtc.f,v $
     . $',                                                             '
     .$Id: urghtc.f,v 1.2 2002/02/11 13:13:15 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,10) NCHAR,(STRING(I),I=1,NCHAR)
10    FORMAT (' ENTER URGHTC - NCHAR=',I2,' STRING=',100A1)
         ENDIF
C
      IF (NCHAR.EQ.0) GO TO 90
C
C  CHECK IF LAST CHARACTER IS NON-BLANK
      IF (STRING(NCHAR).NE.' ') GO TO 70
C
C  FIND FIRST NON-BLANK CHARACTER
      CALL UBEGIN (STRING,NCHAR,IBEG)
      IF (IBEG.EQ.0) THEN
         LBEGIN=0
         GO TO 90
         ENDIF
C
C  FIND LAST NON-BLANK CHARACTER
      CALL ULENTH (STRING,NCHAR,IEND)
C
C  MOVE CHARACTERS TO END OF STRING
      NTIME=IEND
      IGET=IEND
      IPUT=NCHAR
      DO 50 I=1,NTIME
         STRING(IPUT)=STRING(IGET)
         IF (I.LT.NTIME) THEN
            IGET=IGET-1
            IPUT=IPUT-1
            ENDIF
50       CONTINUE
C
C  BLANK REST OF STRING
      IEND=IPUT-1
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (LP,1)
         WRITE (ICMPRU,*) 'IPUT=',IPUT,' IEND=',IEND
         ENDIF
      IF (IEND.GT.0) THEN
         DO 60 I=1,IEND
            IF (ICMDBG.GT.0) THEN
               CALL ULINE (LP,1)
               WRITE (ICMPRU,*) 'I=',I,' IEND=',IEND
               ENDIF
            STRING(I)=' '
60          CONTINUE
         ENDIF
C
C  GET FIRST NON-BLANK CHARACTER
70    CALL UBEGIN (STRING,NCHAR,LBEGIN)
C
90    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,100) LBEGIN,(STRING(I),I=1,NCHAR)
100   FORMAT (' EXIT URGHTC - LBEGIN=',I2,' STRING=',100A1)
         ENDIF
C
      RETURN
C
      END
