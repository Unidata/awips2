C MEMBER USTOP2
C-----------------------------------------------------------------------
C
      SUBROUTINE USTOP2
C
C  ROUTINE TO STOP PROGRAM EXECUTION.
C
      INCLUDE 'ustopx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/ustop2.f,v $
     . $',                                                             '
     .$Id: ustop2.f,v 1.2 2004/05/03 21:39:37 hank Exp $
     . $' /
C    ===================================================================
C
C
C  CHECK STOP CODE
      CALL OFSCLN
      
      IF (ISTOPX.EQ.16) STOP 16
      IF (ISTOPX.EQ.12) STOP 12
      IF (ISTOPX.EQ.8) STOP 8
      IF (ISTOPX.EQ.4) STOP 4
      STOP 0
C
      END
