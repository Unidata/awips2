C MEMBER DGZERO
C  part of old DGPROCU
C-----------------------------------------------------------------------
C
      SUBROUTINE DGZERO (USERID,ISTAT)
C
C  THIS ROUTINE WILL ZERO THE NUMBER OF STATIONS FOR A SPECIFIC USER
C   ON THE NAS-9000 ONLY
C
      REAL*8  USERID
      INTEGER ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/dgzero.f,v $
     . $',                                                             '
     .$Id: dgzero.f,v 1.1 1995/09/17 19:13:23 dws Exp $
     . $' /
C    ===================================================================
C

        ISTAT = 0

      RETURN
      END
