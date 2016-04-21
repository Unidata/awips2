C MEMBER UFXDDN
C-----------------------------------------------------------------------
C
C  ROUTINE TO FILL IN FTXXF001 WHERE XX IS THE DATA SET REFERENCE
C  NUMBER PASSED AS AN INTEGER.
C
      SUBROUTINE UFXDDN (DDNAME,NUNIT,ISTAT)
C
C
      CHARACTER*1 DIGITS(10)/'1','2','3','4','5','6','7','8','9','0'/
      CHARACTER*8 DDNAME
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufxddn.f,v $
     . $',                                                             '
     .$Id: ufxddn.f,v 1.1 1995/09/17 19:02:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,100) DDNAME,NUNIT
         ENDIF
C
      ISTAT=0
C
C  CHECK UNIT NUMBER
      IF (NUNIT.GE.1.AND.NUNIT.LE.99) GO TO 10
         CALL UEROR (LP,1,-1)
         WRITE (LP,110) NUNIT
         ISTAT=1
         GO TO 90
C
10    DDNAME(3:4)='??'
C
C  SET FIRST DIGIT
      IDGT1=NUNIT/10
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,120) IDGT1
         ENDIF
      IF (IDGT1.NE.0) GO TO 20
         NUM=10
         GO TO 40
C
20    DO 30 NUM=1,9
         IF (IDGT1.EQ.NUM) GO TO 40
30       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,140) NUNIT
      ISTAT=2
      GO TO 50
C
40    DDNAME(3:3)=DIGITS(NUM)
C
C  SET SECOND DIGIT
50    IDGT2=NUNIT-IDGT1*10
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,130) IDGT2
         ENDIF
      IF (IDGT2.NE.0) GO TO 60
         NUM=10
         GO TO 80
C
60    DO 70 NUM=1,10
         IF (IDGT2.EQ.NUM) GO TO 80
70       CONTINUE
      CALL UEROR (LP,1,-1)
      WRITE (LP,150) NUNIT
      ISTAT=2
      GO TO 90
C
80    DDNAME(4:4)=DIGITS(NUM)
C
90    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,160) DDNAME
         ENDIF
C
      RETURN
C
C-----------------------------------------------------------------------
C
100   FORMAT (' *** ENTER UFXDDN - DDNAME=',A,3X,'NUNIT=',I3)
110   FORMAT ('+*** ERROR - IN UFXDDN - INVALID UNIT NUMBER - ',I4)
120   FORMAT (' IDGT1=',I2)
130   FORMAT (' IDGT2=',I2)
140   FORMAT ('+*** ERROR - IN UFXDDN - INVALID FIRST DIGIT IN DATA ',
     *   'SET REFERENCE NUMBER (',I3,').')
150   FORMAT ('+*** ERROR - IN UFXDDN - INVALID SECOND DIGIT IN DATA ',
     *   'SET REFERENCE NUMBER (',I3,').')
160   FORMAT (' *** EXIT UFXDDN : DDNAME=',A)
C
      END
