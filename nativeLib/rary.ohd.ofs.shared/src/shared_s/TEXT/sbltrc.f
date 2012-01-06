C MEMBER SBLTRC
C  (from old member SBDBUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:48 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO SET TRACE LEVEL FOR THE SPECIFIED 4-CHARACTER AND/OR
C DESC: 8-CHARACTER DEBUG CODE
C
      SUBROUTINE SBLTRC (CODE4,CODE8,ROUTNE,LTRC)
C
      CHARACTER*4 CODE4
      CHARACTER*8 CODE8,ROUTNE
C
      INCLUDE 'scommon/sudbgx'
C
      EQUIVALENCE (ICVAR1,LDEBUG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sbltrc.f,v $
     . $',                                                             '
     .$Id: sbltrc.f,v 1.1 1995/09/17 19:20:23 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      LDEBUG=0
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,60)
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,70) CODE4,CODE8,ROUTNE
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      LTRC1=0
      LTRC2=0
C
C  SET INDICATOR TO NOT CHECK ROUTINE NAME
      ISDRTN=0
C
C  CHECK IF ANY CODES SPECIFIED
      IF (NSDBUG.EQ.0) GO TO 50
C
C  CHECK IF ROUTINE NAME SPECIFIED
      IF (ROUTNE.EQ.' ') GO TO 40
      IF (CODE4.EQ.' ') GO TO 20
         DO 10 I=1,NSDBUG
            IF (CODE4.NE.SDBUG(I)) GO TO 10
               IF (SDRTN2(I).EQ.' ') GO TO 40
               IF (ROUTNE.EQ.SDRTN2(I)) GO TO 40
               GO TO 20
10          CONTINUE
20    IF (CODE8.EQ.' ') GO TO 40
         DO 30 I=1,NSDBUG
            IF (CODE8.NE.SDBUG2(I)) GO TO 30
               IF (SDRTN2(I).EQ.' ') GO TO 40
               IF (ROUTNE.EQ.SDRTN2(I)) GO TO 40
               GO TO 50
30          CONTINUE
      GO TO 50
C
C  GET TRACE LEVEL FOR 4-CHARACTER CODE
40    IF (CODE4.NE.' ') LTRC1=ISTRC(CODE4)
C
C  GET TRACE LEVEL FOR 8-CHARACTER CODE
      IF (CODE8.NE.' ') LTRC2=ISTRC2(CODE8)
C
C  SET TRACE LEVEL
50    LTRC=LTRC1
      IF (LTRC2.GT.0) LTRC=LTRC2
      ISDRTN=1
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,80) LTRC1,LTRC2,LTRC
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C-----------------------------------------------------------------------
C
60    FORMAT (' *** ENTER SBLTRC')
70    FORMAT (' CODE4=',A4,3X,'CODE8=',A8,3X,'ROUTNE=',A8)
80    FORMAT (' *** EXIT SBLTRC : LTRC1=',I2,3X,'LTRC2=',I2,3X,
     *   'LTRC=',I2)
C
      END
