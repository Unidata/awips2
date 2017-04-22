C MEMBER ISTRC2
C  (from old member SBDBUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:48 BY $WC20SV
C
C @PROCESS LVL(77)
C
      FUNCTION ISTRC2 (CODE)
C
C  ROUTINE TO SET TRACE LEVEL FOR THE SPECIFIED 8-CHARACTER
C  DEBUG CODE.
C
C  RETURN VALUE = >0 IF THE SPECIFIED DEBUG CODE IS FOUND IN THE
C                    DEBUG COMMON BLOCK
C               = 0  OTHERWISE
C
      CHARACTER*8 CODE
C
      INCLUDE 'scommon/sudbgx'
C
      EQUIVALENCE (ICVAR1,LDEBUG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/istrc2.f,v $
     . $',                                                             '
     .$Id: istrc2.f,v 1.1 1995/09/17 19:20:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      LDEBUG=0
C
      IF (LDEBUG.EQ.0) GO TO 20
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         WRITE (IOSDBG,90) IOSDBG,ISTRCE,NSDBUG,ISALL,
     *      ISDBUG,ISDBGL,CODE
         CALL SULINE (IOSDBG,1)
         IF (NSDBUG.EQ.0) GO TO 20
            DO 10 I=1,NSDBUG
               WRITE (IOSDBG,100) I,SDBUG2(I),ISLTRC(I),ISLDBG(I),
     *             SDRTN2(I)
               CALL SULINE (IOSDBG,1)
10             CONTINUE
C
C  CHECK IF ALL DEBUG CODES ARE TO BE ENABLED
20    IF (ISALL.EQ.0) GO TO 30
         ISTRC2=1
         IF (ISTRCE.GT.0) ISTRC2=ISTRCE
         GO TO 70
C
30    ISTRC2=0
C
C  CHECK IF ANY DEBUG CODES ENABELED
      IF (NSDBUG.EQ.0) GO TO 70
C
C  CHECK IF DEBUG CODE SPECIFIED
      DO 40 ICODE=1,NSDBUG
         IF (CODE.EQ.SDBUG2(ICODE)) GO TO 50
40       CONTINUE
         GO TO 70
C
C  CHECK IF ROUTINE NAME TO BE CHECKED
50    IF (ISDRTN.EQ.0) GO TO 60
C
C  CHECK IF ROUTINE NAME SPECIFIED
      IF (SDRTN2(ICODE).NE.' ') GO TO 70
C
C  DEBUG CODE FOUND - SET TRACE LEVEL
60    ISTRC2=ISLTRC(ICODE)
C
70    IF (LDEBUG.GT.0) WRITE (IOSDBG,110) ISTRC2
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER ISTRC2')
90    FORMAT (' IOSDBG=',I2,3X,'ISTRCE=',I2,3X,'NSDBUG=',I2,3X,
     *   'ISALL=',I2,3X,'ISDBUG=',I2,3X,'ISDBGL=',I2,3X,
     *   'CODE=',A8)
100   FORMAT (' I=',I2,3X,'SDBUG2(I)=',A8,3X,'ISLTRC(I)=',I2,3X,
     *   'ISLDBG(I)=',I2,3X,'SDRTN2(I)=',A8)
110   FORMAT (' *** EXIT ISTRC2 : ISTRC2=',I2)
C
      END
