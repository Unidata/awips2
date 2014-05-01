C MODULE PVSUBB
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT MESSAGE WHEN ENTER A ROUTINE.
C
      SUBROUTINE PVSUBB (RTNNAM,ICOND)
C
      CHARACTER*8 RTNNAM
C
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppdutil_dmpsh/RCS/pvsubb.f,v $
     . $',                                                             '
     .$Id: pvsubb.f,v 1.2 2000/07/21 19:53:40 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) WRITE (IOGDB,10) RTNNAM(1:LENSTR(RTNNAM)),ICOND
10    FORMAT (' ENTER ',A,' : ICOND=',I2)
C
      RETURN
C
      END
