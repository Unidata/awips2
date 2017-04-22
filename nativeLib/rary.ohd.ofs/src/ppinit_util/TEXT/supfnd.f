C MEMBER SUPFND
C-----------------------------------------------------------------------
C
C DESC ROUTINE TO CHECK FOR PARIED LEFT AND RIGHT PARENTHESES
C
      SUBROUTINE SUPFND (ILPFND,IRPFND,NPIFLD,NPCFLD)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/supfnd.f,v $
     . $',                                                             '
     .$Id: supfnd.f,v 1.1 1995/09/17 19:15:50 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) WRITE (IOSDBG,30)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG(4HUTIL)
C
      IF (LDEBUG.GT.0) WRITE (IOSDBG,40) ILPFND,IRPFND
      IF (LDEBUG.GT.0) CALL SULINE (IOSDBG,1)
C
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) GO TO 10
         GO TO 20
10    IF (NPCFLD.EQ.0) WRITE (LP,45) NPIFLD
      IF (NPCFLD.GT.0) WRITE (LP,50) NPIFLD,NPCFLD
      CALL SULINE (LP,2)
C
20    ILPFND=0
      IRPFND=0
C
      IF (ISTRCE.GT.1) WRITE (IOSDBG,60)
      IF (ISTRCE.GT.1) CALL SULINE (IOSDBG,1)
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
30    FORMAT (' *** ENTER SUPFND')
40    FORMAT (' ILPFND=',I1,3X,'IRPFND=',I1)
45    FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN INPUT FIELD ',
     *   I2,'.')
50    FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN INPUT FIELD ',
     *   I2,' (CARD FIELD ' ,I2,').')
60    FORMAT (' *** EXIT SUPFND')
C
      END
