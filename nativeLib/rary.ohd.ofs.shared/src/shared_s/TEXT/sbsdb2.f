C MEMBER SBSDB2
C  (from old member SBDBUG)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO SET 8-CHARACTER DEBUG CODE IN COMMON BLOCK SUDBGX
C
      SUBROUTINE SBSDB2 (CODE,ROUTNE,IDBGON,LTRC,LDBG,ISTAT)
C
C
      CHARACTER*8 CODE,ROUTNE
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
      EQUIVALENCE (ICVAR1,LDEBUG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sbsdb2.f,v $
     . $',                                                             '
     .$Id: sbsdb2.f,v 1.1 1995/09/17 19:20:26 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      LDEBUG=0
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,140)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      NFOUND=0
C
C  CHECK FOR BLANK DEBUG CODE
      IF (CODE.NE.' ') GO TO 10
         WRITE (LP,170)
         CALL SULINE (LP,1)
         GO TO 130
C
C  CHECK IF DEBUG ON OR OFF FOR CODE
10    IF (IDBGON.EQ.1) GO TO 80
C  CHECK IF ANY DEBUG CODES SPECIFIED
      IF (NSDBUG.GT.0) GO TO 20
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,150)
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 130
C
C  REMOVE CODE FROM ARRAY
20    DO 30 I=1,NSDBUG
         IF (SDBUG2(I).NE.CODE) GO TO 30
            SDBUG2(I)=' '
            ISLTRC(I)=0
            ISLDBG(I)=0
            SDRTN2(I)=' '
            GO TO 40
30       CONTINUE
      WRITE (LP,180) CODE
      CALL SULINE (LP,2)
      GO TO 130
C
C  COMPRESS ARRAY
40    NDELTE=0
      DO 70 I=1,NSDBUG
         IF (SDBUG2(I).EQ.' ') GO TO 50
            NFOUND=NFOUND+1
            GO TO 70
50       IF (I.EQ.NSDBUG) GO TO 70
         ISTRT=I+1
         DO 60 J=ISTRT,NSDBUG
            IF (SDBUG2(J).EQ.' ') GO TO 60
               SDBUG2(I)=SDBUG2(J)
               SDBUG2(J)=' '
               ISLTRC(I)=ISLTRC(J)
               ISLTRC(J)=0
               ISLDBG(I)=ISLDBG(J)
               ISLDBG(J)=0
               SDRTN2(I)=SDRTN2(J)
               SDRTN2(J)=' '
               NDELTE=NDELTE+1
               GO TO 70
60           CONTINUE
70       CONTINUE
C
      NSDBUG=NSDBUG-NDELTE
      IF (NFOUND.EQ.0) NSDBUG=0
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,200) CODE,NSDBUG
         CALL SULINE (IOSDBG,1)
         ENDIF
      GO TO 130
C
C  CHECK IF ANY CODES CURRENTLY SPECIFIED
80    IF (NSDBUG.EQ.0) GO TO 100
C
C  CHECK IF CODE ALREADY DEFINED
      DO 90 I=1,NSDBUG
         IF (SDBUG2(I).EQ.' ') GO TO 90
         IF (SDBUG2(I).NE.CODE) GO TO 90
            WRITE (LP,190) CODE
            CALL SULINE (LP,2)
            GO TO 120
90      CONTINUE
C
C  CHECK FOR SUFFICIENT SPACE IN DEBUG CODE ARRAY
100   NSDBUG=NSDBUG+1
      IF (NSDBUG.LE.20) GO TO 110
         WRITE (LP,210) CODE
         CALL SULINE (LP,2)
         GO TO 130
C
C  SET DEBUG CODE, TRACE LEVEL AND DEBUG LEVEL
110   SDBUG2(NSDBUG)=CODE
      IVAL=LTRC
      IF (IVAL.LT.0) IVAL=0
      ISLTRC(NSDBUG)=IVAL
      IVAL=LDBG
      IF (IVAL.LT.0) IVAL=0
      ISLDBG(NSDBUG)=IVAL
      IF (ROUTNE.NE.' ') SDRTN2(NSDBUG)=ROUTNE
C
120   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,160) CODE,ROUTNE,NSDBUG
         CALL SULINE (IOSDBG,1)
         ENDIF
C
130   IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,220)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT (' *** ENTER SBSDB2')
150   FORMAT (' CODE ',A,' NOT REMOVED FROM DEBUG ARRAYS ',
     *   'BECAUSE NO CODES CURRENTLY SPECIFIED')
160   FORMAT (' CODE ',A,' AND ROUTINE NAME ',A,
     *   ' SET IN POSITION ',I2,' OF DEBUG ARRAYS')
170   FORMAT (' *** NOTE - BLANK DEBUG CODE FOUND.')
180   FORMAT ('0*** NOTE - DEBUG CODE ',A,' NOT CURRENTLY ',
     *   'SPECIFIED.')
190   FORMAT ('0*** NOTE - DEBUG CODE ',A,' IS ALREADY SPECIFIED.')
200   FORMAT (' *** NOTE - ',A,' DEBUG CODE DELETED. ',I2,
     *   ' DEBUG CODES SPECIFIED.')
210   FORMAT ('0*** WARNING - NOT ENOUGH SPACE IN DEBUG CODE ARRAY TO ',
     *   'STORE CODE ',A,'.')
220   FORMAT (' *** EXIT SBSDB2')
C
      END
