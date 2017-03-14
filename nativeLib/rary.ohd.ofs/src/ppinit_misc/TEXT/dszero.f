C MODULE DSZERO
C  part of old DSPROCU
C-----------------------------------------------------------------------
C
      SUBROUTINE DSZERO (USERID,ISTAT)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/dszero.f,v $
     . $',                                                             '
     .$Id: dszero.f,v 1.2 2000/12/18 22:51:28 dws Exp $
     . $' /
C    ===================================================================
C

        ISTAT = 0

      RETURN
      END
