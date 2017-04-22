C MEMBER UROUND
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
C  ROUTINE TO ROUND A VALUE TO THE NEAREST ROUNDOFF UNIT.
C
      SUBROUTINE UROUND (TYPE,VALUE,ROUND,TROUND,VALUER,ISTAT)
C
C  INPUT ARGUMENTS:
C     TYPE   - TYPE OF DATA VALUE TO BE PROCESSED
C                'INT4'=INTEGER*4
C                'REAL4'=REAL*4
C     VALUE  - DATA VALUE TO BE ROUNDED.
C     ROUND  - ROUNDOFF UNIT.
C     TROUND - INDICATOR FOR HOW ROUNDING IS TO BE DONE.
C                'HIGH'=ROUND TO NEXT HIGHEST VALUE
C                'LOW'=ROUND TO NEXT LOWEST VALUE
C
C  OUTPUT ARGUMENTS:
C     VALUER - DATA VALUE AFTER ROUNDING.
C     ISTAT  - STATUS CODE
C              0=NORMAL RETURN
C              1=INVALID VALUE FOR TYPE OF DATA VALUE
C              2=INVALID VALUE FOR ROUNDOFF UNIT
C
      CHARACTER*(*) TYPE,TROUND
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uround.f,v $
     . $',                                                             '
     .$Id: uround.f,v 1.1 1996/01/17 22:03:39 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** ENTER UROUND'
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'TYPE=',TYPE,
     *      ' TROUND=',TROUND
         ENDIF
C
      ISTAT=0
C
C  CHECK ROUNDOFF TYPE
      IF (TROUND.EQ.'HIGH'.OR.TROUND.EQ.'LOW') THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,30) TROUND
         ENDIF
C
C  CHECK IF PROCESSING INTEGER VALUE
      IF (TYPE.EQ.'INT4') THEN
         CALL SUBSTR (VALUE,1,4,INTVAL,1)
         CALL SUBSTR (ROUND,1,4,INTRND,1)
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'INTVAL=',INTVAL,
     *         'INTRND=',INTRND
            ENDIF
C     CHECK VALUE OF ROUNDOFF UNIT
         IF (INTRND.LE.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,20) INTRND
            ISTAT=2
            GO TO 10
            ENDIF
         IVALUE=0
         IF (TROUND.EQ.'HIGH') THEN
            IF (IVALUE.GE.0) IVALUE=INTVAL+INTRND-1
            IF (IVALUE.LT.0) IVALUE=INTVAL-INTRND+1
            ENDIF
         IF (TROUND.EQ.'LOW') THEN
            IF (IVALUE.GE.0) IVALUE=INTVAL+(INTRND/2)-1
            IF (IVALUE.LT.0) IVALUE=INTVAL-(INTRND/2)+1
            ENDIF
         MULT=IVALUE/INTRND
         INTVAL=MULT*INTRND
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'IVALUE=',IVALUE,
     *         'INTRND=',INTRND,
     *         'MULT=',MULT,
     *         'INTVAL=',INTVAL
            ENDIF
         CALL SUBSTR (INTVAL,1,4,VALUER,1)
         GO TO 10
         ENDIF
C
C  CHECK IF PROCESSING REAL VALUE
      IF (TYPE.EQ.'REAL4') THEN
         CALL SUBSTR (VALUE,1,4,RELVAL,1)
         CALL SUBSTR (ROUND,1,4,RELRND,1)
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'RELVAL=',RELVAL,
     *         'RELRND=',RELRND
            ENDIF
C     CHECK VALUE OF ROUNDOFF UNIT
         IF (RELRND.LE.0.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,20) RELRND
            ISTAT=2
            GO TO 10
            ENDIF
         RVALUE=0.
         IF (TROUND.EQ.'HIGH') THEN
            IF (RVALUE.GE.0.) RVALUE=RELVAL+RELRND
            IF (RVALUE.LT.0.) RVALUE=RELVAL-RELRND
            ENDIF
         IF (TROUND.EQ.'LOW') THEN
            IF (RVALUE.GE.0.) RVALUE=RELVAL+(RELRND/2.)
            IF (RVALUE.LT.0.) RVALUE=RELVAL-(RELRND/2.)
            ENDIF
         MULT=RVALUE/RELRND
         RELVAL=MULT*RELRND
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'RVALUE=',RVALUE,
     *         'RELRND=',RELRND,
     *         'MULT=',MULT,
     *         'RELVAL=',RELVAL
            ENDIF
         CALL SUBSTR (RELVAL,1,4,VALUER,1)
         GO TO 10
         ENDIF
C
      CALL UEROR (LP,1,-1)
      WRITE (LP,40) TYPE
      ISTAT=1
C
10    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) '*** EXIT UROUND'
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT ('+*** ERROR - IN UROUND - ',I5,' IS AN INVALID VALUE ',
     *   'THE ROUNDOFF UNIT. VALUE MUST BE GREATER THAN ZERO.')
30    FORMAT ('+*** ERROR - IN UROUND - ',A,
     *   ' IS AN INVALID ROUNDOFF TYPE. ',
     *   'VALID VALUES ARE ''HIGH'' AND ''LOW''.')
40    FORMAT ('+*** ERROR - IN UROUND - ',A,
     *   ' IS AN INVALID TYPE OF DATA VALUE INDICATOR. ',
     *   'VALID VALUES ARE ''INT4'' AND ''REAL4''.')
C
      END
