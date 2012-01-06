C  =====================================================================
C  pgm: UPRDSA(...)
C  =====================================================================
      SUBROUTINE UPRDSA(DDNAME,NUNIT,DSNAME,IPRERR,LPUNIT,ISTAT)

      CHARACTER*1  DDNAME(8),DSNAME(8)
      INTEGER      NUNIT,IPRERR,LPUNIT,ISTAT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/uprdsa.f,v $
     . $',                                                             '
     .$Id: uprdsa.f,v 1.1 1995/09/17 19:04:35 dws Exp $
     . $' /
C    ===================================================================
C
 
        ISTAT = 0

      RETURN
      END
