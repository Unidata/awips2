C  =====================================================================
C  pgm: UGTRES(...) .. Get time and space from CPU
C  =====================================================================
      SUBROUTINE UGTRES(ICKRGN,MINRGN,NRQRGN,NAVRGN,ICKCPU,MINCPU,NAVCPU
     $,                 IPRERR,IPUNIT,TYPERR,LDEBUG,IERR)

      INTEGER    ICKRGN,MINRGN,NRQRGN,NAVRGN
      INTEGER    ICKCPU,MINCPU,NAVCPU
      INTEGER    IPRERR,IPUNIT,LDEBUG,IERR
      REAL*8     TYPERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/ugtres.f,v $
     . $',                                                             '
     .$Id: ugtres.f,v 1.1 1995/09/17 19:04:34 dws Exp $
     . $' /
C    ===================================================================
C

        IERR = 0

      RETURN
      END
