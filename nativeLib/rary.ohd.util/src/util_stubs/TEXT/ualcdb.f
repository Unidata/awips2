C MEMBER UALCDB
C  =====================================================================
C  PGM: UALCDB .. DUMMY RTN FOR IBM VERSION OF 'OFS'
C  =====================================================================
      SUBROUTINE UALCDB (CNTL,XUSERN,IPRQLF,QLFNEW,TYPMSG,IERR)

      CHARACTER*44 QLFNEW
      CHARACTER*8  TYPMSG,XUSERN
      CHARACTER*4  CNTL
      INTEGER      IPRQLF,IERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/ualcdb.f,v $
     . $',                                                             '
     .$Id: ualcdb.f,v 1.1 1995/09/17 19:04:29 dws Exp $
     . $' /
C    ===================================================================
C

        IERR = 0

      RETURN
      END
