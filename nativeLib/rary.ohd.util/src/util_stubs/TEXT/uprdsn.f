C  =====================================================================
C  pgm: UPRDSN(...)
C  =====================================================================
      SUBROUTINE UPRDSN(DDNAME,NUNIT,DSNAME,IPRERR,LPUNIT,ISTAT)

      CHARACTER*8  DDNAME
      CHARACTER*44 DSNAME
      INTEGER      NUNIT,IPRERR,LPUNIT,ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/uprdsn.f,v $
     . $',                                                             '
     .$Id: uprdsn.f,v 1.1 1995/09/17 19:04:36 dws Exp $
     . $' /
C    ===================================================================
C
 
        ISTAT = 0

      RETURN
      END
