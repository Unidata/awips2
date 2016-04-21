C  ============================================================================
      SUBROUTINE HCKDAT (IS,IEX,ITBUF,IERR)

      INTEGER    IS,IEX,ITBUG(7),IERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_stubs/RCS/hckdat.f,v $
     . $',                                                             '
     .$Id: hckdat.f,v 1.1 1997/07/30 13:42:47 page Exp $
     . $' /
C    ===================================================================
C

        ITBUG(1) = 0
        ITBUG(2) = 0
        ITBUG(3) = 0
        ITBUG(4) = 0
        ITBUG(5) = 0
        ITBUG(6) = 0
        ITBUG(7) = 0

        IERR = 0

      RETURN
      END
