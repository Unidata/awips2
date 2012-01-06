C MODULE SUWARN
C-----------------------------------------------------------------------
C
      SUBROUTINE SUWARN
C
C  ROUTINE SUWARN COUNTS THE NUMBER OF WARNINGS MESSAGES.
C
      CHARACTER*1 CARCTL
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/supagx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/suwarn.f,v $
     . $',                                                             '
     .$Id: suwarn.f,v 1.3 2001/06/13 13:32:42 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'ENTER SUWARN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      NWARN=NWARN+1
      NTWARN=NTWARN+1
      NRWARN=NRWARN+1
C
C  STORE PAGE ON WHICH WARNING OCCURRED
      DO 10 I=1,MPGWRN
         IF (LPGWRN(I).EQ.NPSPAG) GO TO 20
         IF (LPGWRN(I).NE.0) GO TO 10
            LPGWRN(I)=NPSPAG
            NPGWRN=NPGWRN+1
            GO TO 20
10       CONTINUE
C
20    IF (ICMCDE.LT.4) ICMCDE=4
      IF (KLCODE.LT.4) KLCODE=4
C
C  CHECK IF MAXIMUM WARNINGS ALLOWED EXCEEDED
      IF (NTWARN.GT.IOPMXW) THEN
         WRITE (LP,40)
         CALL SULINE (LP,2)
         CARCTL='+'
         WRITE (LP,50) CARCTL,IOPMXW
         IF (IOPOVP.EQ.1) THEN
            WRITE (LP,50) CARCTL,IOPMXW
            WRITE (LP,50) CARCTL,IOPMXW
            ENDIF
         CALL SULINE (LP,0)
         IF (IOPCLG(1).EQ.1) THEN
            IUNIT=IOPCLG(2)
            WRITE (IUNIT,50) CARCTL,IOPMXW
            ENDIF
         ICMCDE=16
         KLCODE=16
         CALL SUEND
         STOP 16
         ENDIF
C
      IF (ISTRCE.GE.3) THEN
         WRITE (IOSDBG,*) 'EXIT SUWARN'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT ('0')
50    FORMAT (A,'*** FATAL ERROR - MAXIMUM NUMBER OF WARNINGS ALLOWED ',
     *   '(',I4,') EXCEEDED. PROGRAM EXECUTION STOPPED.')
C
      END
