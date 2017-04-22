C MEMBER ISBUG2
C  (from old member SBDBUTIL)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/05/95.10:36:48 BY $WC20SV
C
C @PROCESS LVL(77)
C
      FUNCTION ISBUG2 (CODE)
C
C  ROUTINE TO SET DEBUG LEVEL FOR THE SPECIFIED 8-CHARACTER
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/isbug2.f,v $
     . $',                                                             '
     .$Id: isbug2.f,v 1.1 1995/09/17 19:20:16 dws Exp $
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
         ISBUG2=1
         IF (ISDBGL.GT.0) ISBUG2=ISDBGL
         GO TO 70
C
30    ISBUG2=0
C
C  CHECK IF ANY DEBUG CODES ENABELED
      IF (NSDBUG.EQ.0) GO TO 70
C
C  CHECK IF DEBUG CODE ENABELED
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
C  DEBUG CODE FOUND - SET DEBUG LEVEL
60    ISBUG2=ISLDBG(ICODE)
C
70    IF (LDEBUG.GT.0) WRITE (IOSDBG,110) ISBUG2
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER ISBUG2')
90    FORMAT (' IOSDBG=',I2,3X,'ISBUG2E=',I2,3X,'NSDBUG=',I2,3X,
     *   'ISALL=',I2,3X,'ISDBUG=',I2,3X,'ISDBGL=',I2,3X,
     *   'CODE=',A8)
100   FORMAT (' I=',I2,3X,'SDBUG2(I)=',A4,3X,'ISLTRC(I)=',I2,3X,
     *   'ISLDBG(I)=',I2,3X,'SDRTN2(I)=',A8)
110   FORMAT (' *** EXIT ISBUG2 : ISBUG2=',I2)
C
      END
