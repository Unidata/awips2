C MODULE PVSUBE
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT MESSAGE WHEN EXIT A ROUTINE.
C
      SUBROUTINE PVSUBE (RTNNAM,ICOND)
C
      CHARACTER*8 RTNNAM
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvsube.f,v $
     . $',                                                             '
     .$Id: pvsube.f,v 1.2 2000/07/21 19:53:50 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10) RTNNAM(1:LENSTR(RTNNAM)),ICOND
10    FORMAT (' EXIT ',A,' : ICOND=',I2)
C
      RETURN
C
      END
