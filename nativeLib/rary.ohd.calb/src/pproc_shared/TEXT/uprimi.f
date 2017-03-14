C MODULE UPRIMI
C  =====================================================================
      SUBROUTINE UPRIMI(JREA,JWRI,JPUN,JDEB,JERR,JSYS)

      INTEGER   JREA,JWRI,JPUN,JDEB,JERR,JSYS

      INCLUDE 'uio'
      COMMON  /UIOX/ LP2,ICD2,LPD2,LPE2,ICDPU2,LSYS2
      INCLUDE 'ucmdbx'
      INCLUDE 'udebug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      COMMON  /ERRDAT/IOERR2,NWARN2,NERRS2
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pproc_shared/RCS/uprimi.f,v $
     . $',                                                             '
     .$Id: uprimi.f,v 1.3 1999/07/07 16:45:58 page Exp $
     . $' /
C    ===================================================================
C

C         for 'uiox'

        LP2    = JWRI
        ICD2   = JREA
        LPD2   = JDEB
        LPE2   = JERR
        ICDPU2 = JPUN
        LSYS2  = JSYS

C         for 'uio'

        LP     = JWRI
        ICD    = JREA
        LPD    = JDEB
        LPE    = JERR
        ICDPUN = JPUN

C         for 'ucmdbx'

        ICMPRU = JWRI

C         for 'udebug'

        IOGDB  = JWRI

C         for 'common/ionum'

        IN     = JREA
        IPR    = JWRI
        IPU    = JPUN

C         for 'common/fdbug'

        IODBUG = JWRI

C         for 'common/errdat'

        IOERR2 = JWRI

C         for 'scommon/sudbgx'

        IOSDBG = JWRI

C         for 'scommon/suerrx'

        IOERR  = JWRI

      RETURN
      END
